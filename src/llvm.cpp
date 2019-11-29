

#include "llvm.h"
#include "ast.h"
#include "compiler.h"

// We dont need or care about a wall of warnings from LLVM code.
#ifdef WIN32
#pragma warning(push, 0)
#endif

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/IRBuilder.h"

#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/ExecutionEngine/JITSymbol.h"

#include "llvm/Transforms/Utils/Cloning.h"

#ifdef WIN32
#pragma warning(pop)
#endif

using namespace llvm;
using namespace llvm::orc;

static StringRef string_ref(String s) {
    return StringRef(s.data, s.length);
}

DIFile *get_debug_file(LLVMContext *ctx, Ast *ast) {
    return DIFile::get(*ctx, string_ref(ast->filename), "");
}

string_length_type get_line_number(Ast *ast) {
    // @Speed we should probably just track line_start on Ast itself
    // and have the lexer set this on tokens.
    string_length_type line_start, char_start, line_end, char_end;
    ast->text_span.calculate_text_coordinates(&line_start, &char_start, &line_end, &char_end);

    return line_start;
}

void LLVM_Generator::preinit() {
    // This gets called before Compiler is fully initialized so that Compiler
    // can query TargetMachine to configure string and arrays to be the right
    // size on 32-bit vs 64-bit machines.
    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();
    
    std::string TargetTriple = llvm::sys::getDefaultTargetTriple();
    if (compiler->build_options.target_triple.length) {
        TargetTriple = to_c_string(compiler->build_options.target_triple); // @Leak
    }
    // printf("TRIPLE: '%s'\n", TargetTriple.c_str());
    
    std::string Error;
    auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);
    
    // Print an error and exit if we couldn't find the requested target.
    // This generally occurs if we've forgotten to initialise the
    // TargetRegistry or we have a bogus target triple.
    if (!Target) {
        compiler->report_error((Ast *)nullptr, "LLVM error: %s\n", Error.c_str());
        return;
    }
    
    auto CPU = "generic";
    auto Features = "";
    
    TargetOptions opt;
    auto RM = Optional<Reloc::Model>();
    // RM = Reloc::Model::PIC_;
    TargetMachine = Target->createTargetMachine(TargetTriple, CPU, Features, opt, RM);
}

void LLVM_Generator::init() {
    auto ctx = llvm::make_unique<LLVMContext>();
    thread_safe_context = new ThreadSafeContext(std::move(ctx));
    llvm_context = thread_safe_context->getContext();
    
    llvm_module = new Module("jiyu Module", *llvm_context);

    irb = new IRBuilder<>(*llvm_context);
    dib = new DIBuilder(*llvm_module);

    const char *JIYU_PRODUCER_STRING = "Jiyu Compiler";
    bool is_optimized = false; // @BuildOptions
    const char *COMMAND_LINE_FLAGS = "";
    const unsigned runtime_version = 0;
    di_compile_unit = dib->createCompileUnit(dwarf::DW_LANG_C, DIFile::get(*llvm_context, "fib.jyu", ""), JIYU_PRODUCER_STRING, is_optimized, COMMAND_LINE_FLAGS, runtime_version);
    
    type_void = Type::getVoidTy(*llvm_context);
    type_i1   = Type::getInt1Ty(*llvm_context);
    type_i8   = Type::getInt8Ty(*llvm_context);
    type_i16  = Type::getInt16Ty(*llvm_context);
    type_i32  = Type::getInt32Ty(*llvm_context);
    type_i64  = Type::getInt64Ty(*llvm_context);
    
    type_f32  = Type::getFloatTy(*llvm_context);
    type_f64  = Type::getDoubleTy(*llvm_context);
    
    type_string_length = nullptr;
    if (TargetMachine->getPointerSize(0) == 4) {
        type_string_length = type_i32;
        type_intptr = type_i32;
    } else if (TargetMachine->getPointerSize(0) == 8) {
        type_string_length = type_i64;
        type_intptr = type_i64;
    }
    
    assert(type_string_length);
    
    // Matches the definition in general.h, except when the target's pointer size doesn't match the host's.
    type_string = StructType::create(*llvm_context, { type_i8->getPointerTo(), type_string_length }, "string", false/*packed*/);

    di_type_bool = dib->createBasicType("bool",    8, dwarf::DW_ATE_boolean);
    di_type_s8   = dib->createBasicType("int8",    8, dwarf::DW_ATE_signed);
    di_type_s16  = dib->createBasicType("int16",  16, dwarf::DW_ATE_signed);
    di_type_s32  = dib->createBasicType("int32",  32, dwarf::DW_ATE_signed);
    di_type_s64  = dib->createBasicType("int64",  64, dwarf::DW_ATE_signed);
    di_type_u8   = dib->createBasicType("uint8",   8, dwarf::DW_ATE_unsigned);
    di_type_u16  = dib->createBasicType("uint16", 16, dwarf::DW_ATE_unsigned);
    di_type_u32  = dib->createBasicType("uint32", 32, dwarf::DW_ATE_unsigned);
    di_type_u64  = dib->createBasicType("uint64", 64, dwarf::DW_ATE_unsigned);
    di_type_f32  = dib->createBasicType("float",  32, dwarf::DW_ATE_float);
    di_type_f64  = dib->createBasicType("double", 64, dwarf::DW_ATE_float);

    di_type_string_length = nullptr;
    if (TargetMachine->getPointerSize(0) == 4) {
        di_type_string_length = di_type_s32;
    } else if (TargetMachine->getPointerSize(0) == 8) {
        di_type_string_length = di_type_s64;
    }

    {
        auto debug_file = DIFile::get(*llvm_context, "", "");
        unsigned line_number = 0;
        DINode::DIFlags flags = DINode::DIFlags();

        auto di_data_type  = dib->createPointerType(di_type_u8, TargetMachine->getPointerSizeInBits(0));

        // @Cleanup literal numbers
        auto data = dib->createMemberType(di_compile_unit, "data", debug_file, line_number,
                            TargetMachine->getPointerSizeInBits(0), TargetMachine->getPointerSizeInBits(0),
                            0, flags, di_data_type);
        auto length = dib->createMemberType(di_compile_unit, "length", debug_file, line_number,
                            type_string_length->getPrimitiveSizeInBits(), type_string_length->getPrimitiveSizeInBits(),
                            TargetMachine->getPointerSizeInBits(0), flags, di_type_string_length);
        auto elements = dib->getOrCreateArray({data, length});

        auto type = compiler->type_string;
        di_type_string = dib->createStructType(di_compile_unit, "string", debug_file,
                            line_number, type->size * BYTES_TO_BITS, type->alignment * BYTES_TO_BITS,
                            flags, nullptr, elements);
    }

    {
        auto debug_file = DIFile::get(*llvm_context, "", "");
        unsigned line_number = 0;
        DINode::DIFlags flags = DINode::DIFlags();

        auto info = compiler->type_info_type;
        auto elements = dib->getOrCreateArray({});
        di_type_type = dib->createStructType(di_compile_unit, "Type", debug_file, line_number,
                        info->size * BYTES_TO_BITS, info->alignment * BYTES_TO_BITS, flags, nullptr, elements);
    }

    di_current_scope = di_compile_unit;

    llvm_types.resize(compiler->type_table.count);

    for (auto entry: compiler->type_table) {
        if (llvm_types[entry->type_table_index]) continue;

        if (types_match(entry, compiler->type_void)) {
            llvm_types[entry->type_table_index] = type_void;
            continue;
        }

        llvm_types[entry->type_table_index] = make_llvm_type(entry);
    }

    // @Incomplete do this for llvm_debug_types as well..
}

void LLVM_Generator::finalize() {
    dib->finalize();

    std::string TargetTriple = TargetMachine->getTargetTriple().str();
    // printf("TRIPLE: %s\n", TargetTriple.c_str());
    
    llvm_module->setDataLayout(TargetMachine->createDataLayout());
    llvm_module->setTargetTriple(TargetTriple);

    bool is_win32 = TargetMachine->getTargetTriple().isOSWindows();
    if (is_win32) {
        llvm_module->addModuleFlag(Module::Warning, "CodeView", 1);
    }
    
    String exec_name = compiler->build_options.executable_name;
    String obj_name = mprintf("%.*s.o", exec_name.length, exec_name.data);

    std::error_code EC;
    raw_fd_ostream dest(string_ref(obj_name), EC, sys::fs::F_None);
    
    if (EC) {
        compiler->report_error((Ast *)nullptr, "Could not open file: %s\n", EC.message().c_str());
        return;
    }
    
    legacy::PassManager pass;
    auto FileType = TargetMachine::CGFT_ObjectFile;
    
    // llvm_module->dump();
    
    pass.add(createVerifierPass(false));
    if (TargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
        compiler->report_error((Ast *)nullptr, "TargetMachine can't emit a file of this type"); // @TODO this error message is unclear for the user.
        return;
    }
    
    pass.run(*llvm_module);
    dest.flush();

    free(obj_name.data);
}

Type *LLVM_Generator::get_type(Ast_Type_Info *type) {
    return llvm_types[type->type_table_index];
}

Type *LLVM_Generator::make_llvm_type(Ast_Type_Info *type) {
    type = get_final_type(type);

    if (type->type == Ast_Type_Info::VOID) {
        // return type_i8 for pointers.
        return type_i8;
    }

    if (type->type == Ast_Type_Info::TYPE) {
        return type_i8->getPointerTo();
    }
    
    if (type->type == Ast_Type_Info::INTEGER) {
        switch (type->size) {
            case 1: return type_i8;
            case 2: return type_i16;
            case 4: return type_i32;
            case 8: return type_i64;
            default: assert(false);
        }
    }
    
    if (type->type == Ast_Type_Info::BOOL) {
        return type_i1;
    }
    
    if (type->type == Ast_Type_Info::FLOAT) {
        switch(type->size) {
            case 4: return type_f32;
            case 8: return type_f64;
            default: assert(false);
        }
    }
    
    if (type->type == Ast_Type_Info::STRING) {
        return type_string;
    }
    
    if (type->type == Ast_Type_Info::POINTER) {
        auto pointee = make_llvm_type(type->pointer_to);
        return pointee->getPointerTo();
    }
    
    if (type->type == Ast_Type_Info::ARRAY) {
        auto element = make_llvm_type(type->array_element);
        if (type->array_element_count >= 0) {
            assert(type->is_dynamic == false);
            
            return ArrayType::get(element, type->array_element_count);
        } else {
            // @Cleanup this should be type_array_count or something
            auto count = type_string_length;
            auto data  = element->getPointerTo();
            
            if (!type->is_dynamic) {
                return StructType::get(*llvm_context, {data, count}, false);
            } else {
                auto allocated = count;
                return StructType::get(*llvm_context, {data, count, allocated}, false);
            }
        }
    }
    
    if (type->type == Ast_Type_Info::STRUCT) {
        // Prevent recursion.
        if (llvm_types[type->type_table_index]) {
            return llvm_types[type->type_table_index];
        }

        auto final_type = StructType::create(*llvm_context, type->struct_decl->identifier ? string_ref(type->struct_decl->identifier->name->name) : "");
        llvm_types[type->type_table_index] = final_type;

        Array<Type *> member_types;
        
        for (auto member : type->struct_members) {
            if (member.is_let) continue;
            
            member_types.add(make_llvm_type(member.type_info));
        }

        final_type->setBody(ArrayRef<Type *>(member_types.data, member_types.count), false/*is packed*/);

        // final_type->dump();
        
        return final_type;
    }

    if (type->type == Ast_Type_Info::ENUM) {
        // createEnumerationType(DIScope *   Scope,
        //     StringRef   Name,
        //     DIFile *    File,
        //     unsigned    LineNumber,
        //     uint64_t    SizeInBits,
        //     uint32_t    AlignInBits,
        //     DINodeArray     Elements,
        //     DIType *    UnderlyingType,
        //     StringRef   UniqueIdentifier = "",
        //     bool    IsScoped = false 
        // )

        // @@ TODO! Do we need to add info about enum members to the Ast_Type_Info?
        return make_llvm_type(type->enum_base_type);
    }
    
    if (type->type == Ast_Type_Info::FUNCTION) {
        Array<Type *> arguments;
        
        bool is_c_function = type->is_c_function;
        bool is_win32 = TargetMachine->getTargetTriple().isOSWindows();
        
        for (auto arg_type : type->arguments) {
            if (arg_type == compiler->type_void) continue;
            
            Type *type = make_llvm_type(arg_type);
            
            if (is_c_function && is_win32 && is_aggregate_type(arg_type)) {
                assert(arg_type->size >= 0);
                
                // @TargetInfo this is only true for x64 too
                const int _8BYTES = 8;
                if (arg_type->size > _8BYTES) {
                    arguments.add(type->getPointerTo());
                    continue;
                }
            } else if (is_aggregate_type(arg_type)) {
                arguments.add(type->getPointerTo());
                continue;
            }
            
            arguments.add(type);
        }
        
        Type *return_type = make_llvm_type(type->return_type);
        if (type->return_type->type == Ast_Type_Info::VOID) {
            return_type = type_void;
        }
        
        return FunctionType::get(return_type, ArrayRef<Type *>(arguments.data, arguments.count), type->is_c_varargs)->getPointerTo();
    }
    
    assert(false);
    return nullptr;
}

DIType *LLVM_Generator::get_debug_type(Ast_Type_Info *type) {
    type = get_final_type(type);

    if (type->type == Ast_Type_Info::VOID) {
        return nullptr;
    }
    
    if (type->type == Ast_Type_Info::INTEGER) {
        if (type->is_signed) {
            switch (type->size) {
                case 1: return di_type_s8;
                case 2: return di_type_s16;
                case 4: return di_type_s32;
                case 8: return di_type_s64;
                default: assert(false);
            }
        } else {
            switch (type->size) {
                case 1: return di_type_u8;
                case 2: return di_type_u16;
                case 4: return di_type_u32;
                case 8: return di_type_u64;
                default: assert(false);
            }
        }
    }
    
    if (type->type == Ast_Type_Info::BOOL) {
        return di_type_bool;
    }
    
    if (type->type == Ast_Type_Info::FLOAT) {
        switch(type->size) {
            case 4: return di_type_f32;
            case 8: return di_type_f64;
            default: assert(false);
        }
    }
    
    if (type->type == Ast_Type_Info::STRING) {
        return di_type_string;
    }
    
    if (type->type == Ast_Type_Info::POINTER) {
        auto pointee = get_debug_type(type->pointer_to);
        return dib->createPointerType(pointee, TargetMachine->getPointerSizeInBits(0));
    }
    
    if (type->type == Ast_Type_Info::ARRAY) {
        auto element = get_debug_type(type->array_element);
        if (type->array_element_count >= 0) {
            assert(type->is_dynamic == false);
            
            auto subscripts = dib->getOrCreateArray({ dib->getOrCreateSubrange(0, type->array_element_count) });
            return dib->createArrayType(type->size * BYTES_TO_BITS, type->alignment * BYTES_TO_BITS, element, subscripts);
        } else {
            // @Cleanup this should be type_array_count or something
            auto di_count_type = di_type_string_length;
            auto di_data_type  = dib->createPointerType(element, TargetMachine->getPointerSizeInBits(0));

            auto debug_file = DIFile::get(*llvm_context, "", "");

            DINode::DIFlags flags = DINode::DIFlags();

            // @Cleanup literal numbers
            auto data = dib->createMemberType(di_compile_unit, "data", debug_file, 0,
                                TargetMachine->getPointerSizeInBits(0), TargetMachine->getPointerSizeInBits(0),
                                0, flags, di_data_type);
            auto count = dib->createMemberType(di_compile_unit, "count", debug_file, 0,
                                type_string_length->getPrimitiveSizeInBits(), type_string_length->getPrimitiveSizeInBits(),
                                TargetMachine->getPointerSizeInBits(0), flags, di_count_type);
            
            unsigned line_number = 0;
            DIType *derived_from = nullptr;
            if (!type->is_dynamic) {
                auto elements = dib->getOrCreateArray({data, count});
                // @Incomplete we should probably be using the correct scope info here.
                // @Incomplete name
                String name;
                return dib->createStructType(di_compile_unit, string_ref(name), debug_file,
                                    line_number, type->size * BYTES_TO_BITS, type->alignment * BYTES_TO_BITS,
                                    flags, derived_from, elements);
            } else {
                auto allocated = dib->createMemberType(di_compile_unit, "allocated", debug_file, 0,
                                type_string_length->getPrimitiveSizeInBits(), type_string_length->getPrimitiveSizeInBits(),
                                TargetMachine->getPointerSizeInBits(0) + type_string_length->getPrimitiveSizeInBits(), flags, di_count_type);
                auto elements = dib->getOrCreateArray({data, count, allocated});
                // @Incomplete we should probably be using the correct scope info here.
                // @Incomplete name
                String name;
                return dib->createStructType(di_compile_unit, string_ref(name), debug_file,
                                    line_number, type->size * BYTES_TO_BITS, type->alignment * BYTES_TO_BITS,
                                    flags, derived_from, elements);
            }
        }
    }
    
    if (type->type == Ast_Type_Info::STRUCT) {
        if (type->debug_type_table_index >= 0) {
            return llvm_debug_types[type->debug_type_table_index];
        }

        auto struct_decl = type->struct_decl;
        auto debug_file = get_debug_file(llvm_context, struct_decl);
        auto line_number = get_line_number(struct_decl);

        String name;
        if (struct_decl->identifier) name = struct_decl->identifier->name->name;
        DIType *derived_from = nullptr;
        DINode::DIFlags flags = DINode::DIFlags();
        auto empty_elements = dib->getOrCreateArray({});
        auto final_type = dib->createStructType(di_compile_unit, string_ref(name), debug_file,
                            line_number, type->size * BYTES_TO_BITS, type->alignment * BYTES_TO_BITS,
                            flags, derived_from, empty_elements);

        type->debug_type_table_index = llvm_debug_types.count;
        llvm_debug_types.add(final_type);

        Array<Metadata *> member_types;
        for (auto member : type->struct_members) {
            if (member.is_let) continue;
            
            auto member_type = member.type_info;
            auto di_type = get_debug_type(member_type);

            String name;
            if (member.name) name = member.name->name;

            DINode::DIFlags flags = DINode::DIFlags();
            // @Incomplete file and line number are not technically correct.
            auto di_member = dib->createMemberType(di_compile_unit, string_ref(name), debug_file, line_number,
                                        member_type->size * BYTES_TO_BITS, member_type->alignment * BYTES_TO_BITS,
                                        member.offset_in_struct * BYTES_TO_BITS, flags, di_type);
            member_types.add(di_member);
        }

        
        auto elements = dib->getOrCreateArray(ArrayRef<Metadata *>(member_types.data, member_types.count));
        final_type->replaceElements(elements);

        return final_type;
    }
    
    if (type->type == Ast_Type_Info::FUNCTION) {
        auto subroutine_type = get_debug_subroutine_type(type);

        // Always return a pointer here. Code that doesn't want a pointer should call get_debug_subroutine_type directly.
        return dib->createPointerType(subroutine_type, TargetMachine->getPointerSizeInBits(0));
    }

    if (type->type == Ast_Type_Info::TYPE) {
        return di_type_type;
    }
    
    assert(false);
    return nullptr;
}

FunctionType *LLVM_Generator::create_function_type(Ast_Function *function) {
    Type *type = get_type(get_type_info(function));
    assert(type->isPointerTy());
    
    type = type->getPointerElementType();
    assert(type->isFunctionTy());
    return static_cast<FunctionType *>(type);
}

Value *LLVM_Generator::get_value_for_decl(Ast_Declaration *decl) {
    for (auto &it : decl_value_map) {
        if (it.item1 == decl) {
            return it.item2;
        }
    }
    
    return nullptr;
}

static Value *create_alloca_in_entry(IRBuilder<> *irb, Type *type) {
    auto current_block = irb->GetInsertBlock();
    auto current_debug_loc = irb->getCurrentDebugLocation();
    
    auto func = current_block->getParent();
    
    BasicBlock *entry = &func->getEntryBlock();
    irb->SetInsertPoint(entry->getTerminator());
    
    Value *alloca = irb->CreateAlloca(type);
    
    irb->SetInsertPoint(current_block);
    irb->SetCurrentDebugLocation(current_debug_loc);
    
    return alloca;
}

Value *LLVM_Generator::create_string_literal(Ast_Literal *lit, bool want_lvalue) {
    assert(lit->literal_type == Ast_Literal::STRING);
    
    Value *value = nullptr;
    if (lit->string_value.length == 0 || lit->string_value.data == nullptr) {
        value = Constant::getNullValue(type_string);
    } else {
        bool add_null = true;
        Constant *data = irb->CreateGlobalStringPtr(string_ref(lit->string_value));
        Constant *length = ConstantInt::get(type_string_length, lit->string_value.length);
        
        value = ConstantStruct::get(type_string, { data, length });
    }
    
    assert(value);
    
    if (want_lvalue) {
        auto alloca = create_alloca_in_entry(irb, type_string);
        irb->CreateStore(value, alloca);
        return alloca;
    }

    return value;
}

Value *LLVM_Generator::dereference(Value *value, s64 element_path_index, bool is_lvalue) {
    // @TODO I think ideally, the front-end would change and dereferences of constant values with replaecments of literals of the value so that we can simplify LLVM code generation
    if (auto constant = dyn_cast<ConstantAggregate>(value)) {
        return irb->CreateExtractValue(constant, element_path_index);
    } else {
        // @Cleanup type_i32 use for array indexing?
        auto valueptr = irb->CreateGEP(value, { ConstantInt::get(type_i32, 0), ConstantInt::get(type_i32, element_path_index) });
        if (!is_lvalue) return irb->CreateLoad(valueptr);
        return valueptr;
    }
}

void LLVM_Generator::default_init_struct(Value *decl_value, Ast_Type_Info *info) {
    info = get_final_type(info);

    assert(info->type == Ast_Type_Info::STRUCT);
    assert(info->struct_decl);
    
    auto _struct = info->struct_decl;
    
    auto null_value = Constant::getNullValue(get_type(info));
    assert(null_value->getType() == decl_value->getType()->getPointerElementType());
    irb->CreateStore(null_value, decl_value);
    
    s32 element_path_index = 0;
    for (auto member: _struct->member_scope.declarations) {
        if (member->type == AST_DECLARATION) {
            auto decl = static_cast<Ast_Declaration *>(member);
            
            if (decl->is_let) continue;
            assert(decl->is_struct_member);
            
            if (decl->initializer_expression) {
                auto expr = emit_expression(decl->initializer_expression);
                
                auto gep = dereference(decl_value, element_path_index, true);
                irb->CreateStore(expr, gep);
            } else {
                auto mem_info = get_type_info(decl);
                if (is_struct_type(mem_info)) {
                    auto gep = dereference(decl_value, element_path_index, true);
                    default_init_struct(gep, mem_info);
                }
            }
            
            element_path_index++;
        }
    }
}

Value *LLVM_Generator::emit_expression(Ast_Expression *expression, bool is_lvalue) {
    while(expression->substitution) expression = expression->substitution;
    
    switch (expression->type) {
        case AST_SCOPE: {
            auto scope = static_cast<Ast_Scope *>(expression);
            emit_scope(scope);
            
            return nullptr;
        }
        
        case AST_SCOPE_EXPANSION: {
            auto exp = static_cast<Ast_Scope_Expansion *>(expression);

            if (exp->expanded_via_import_directive) return nullptr;

            emit_scope(exp->scope);
            return nullptr;
        }
        
        case AST_UNARY_EXPRESSION: {
            auto un = static_cast<Ast_Unary_Expression *>(expression);
            
            if (un->operator_type == Token::STAR) {
                auto value = emit_expression(un->expression, true);
                return value;
            } else if (un->operator_type == Token::DEREFERENCE_OR_SHIFT) {
                auto value = emit_expression(un->expression, is_lvalue);
                
                value = irb->CreateLoad(value);
                return value;
            } else if (un->operator_type == Token::MINUS) {
                auto value = emit_expression(un->expression);
                auto type = get_type_info(un->expression);
                
                if (is_int_type(type)) {
                    return irb->CreateNeg(value);
                } else if (is_float_type(type)) {
                    return irb->CreateFNeg(value);
                }
            }
            
            assert(false);
            break;
        }
        
        case AST_BINARY_EXPRESSION: {
            auto bin = static_cast<Ast_Binary_Expression *>(expression);
            
            if (bin->operator_type == Token::EQUALS) {
                Value *left  = emit_expression(bin->left,  true);
                Value *right = emit_expression(bin->right, false);
                
                irb->CreateStore(right, left);
                return nullptr;
            } else {
                Value *left  = emit_expression(bin->left,  false);
                Value *right = emit_expression(bin->right, false);
                
                // @TODO NUW NSW?
                switch (bin->operator_type) {
                    case Token::STAR: {
                        auto info = get_type_info(bin->left);
                        if (is_int_type(info)) {
                            return irb->CreateMul(left, right);
                        } else {
                            assert(is_float_type(info));
                            return irb->CreateFMul(left, right);
                        }
                    }
                    case Token::PERCENT: {
                        auto info = get_type_info(bin->left);
                        if (is_int_type(info)) {
                            if (info->is_signed) {
                                return irb->CreateSRem(left, right);
                            } else {
                                return irb->CreateURem(left, right);
                            }
                        } else {
                            assert(is_float_type(info));
                            return irb->CreateFRem(left, right);
                        }
                    }
                    case Token::SLASH: {
                        auto info = get_type_info(bin->left);
                        if (is_int_type(info)) {
                            if (info->is_signed) {
                                return irb->CreateSDiv(left, right);
                            } else {
                                return irb->CreateUDiv(left, right);
                            }
                        } else {
                            assert(is_float_type(info));
                            return irb->CreateFDiv(left, right);
                        }
                    }

                    case Token::PLUS: {
                        auto left_type = get_type_info(bin->left);
                        auto right_type = get_type_info(bin->right);
                        
                        if (is_pointer_type(left_type) &&
                            is_int_type(right_type)) {
                            return irb->CreateGEP(left, {right});
                        } else if (is_pointer_type(left_type) && is_pointer_type(right_type)) {
                            Value *left_int  = irb->CreatePtrToInt(left,  type_intptr);
                            Value *right_int = irb->CreatePtrToInt(right, type_intptr);
                            
                            Value *result = irb->CreateAdd(left_int, right_int);
                            return irb->CreateIntToPtr(result, get_type(left_type));
                        } else if (is_float_type(left_type)) {
                            assert(is_float_type(right_type));
                            
                            return irb->CreateFAdd(left, right);
                        }
                        
                        return irb->CreateAdd(left, right);
                    }
                    case Token::MINUS: {
                        auto left_type  = get_type_info(bin->left);
                        auto right_type = get_type_info(bin->right);
                        
                        if (is_pointer_type(left_type) && is_pointer_type(right_type)) {
                            Value *left_int  = irb->CreatePtrToInt(left,  type_intptr);
                            Value *right_int = irb->CreatePtrToInt(right, type_intptr);
                            
                            Value *result = irb->CreateSub(left_int, right_int);
                            return irb->CreateIntToPtr(result, get_type(left_type));
                        }
                        
                        if (is_float_type(left_type) && is_float_type(right_type)) {
                            return irb->CreateFSub(left, right);
                        }
                        
                        return irb->CreateSub(left, right);
                    }
                    case Token::EQ_OP: {
                        return irb->CreateICmpEQ(left, right);
                    }
                    case Token::NE_OP: return irb->CreateICmpNE(left, right);
                    case Token::LE_OP: {
                        auto info = get_type_info(bin->left);
                        if (is_int_type(info)) {
                            if (info->is_signed) {
                                return irb->CreateICmpSLE(left, right);
                            } else {
                                return irb->CreateICmpULE(left, right);
                            }
                        } else {
                            assert(is_float_type(info));
                            return irb->CreateFCmpULE(left, right);
                        }
                    }
                    case Token::GE_OP: {
                        auto info = get_type_info(bin->left);
                        if (is_int_type(info)) {
                            if (info->is_signed) {
                                return irb->CreateICmpSGE(left, right);
                            } else {
                                return irb->CreateICmpUGE(left, right);
                            }
                        } else {
                            assert(is_float_type(info));
                            return irb->CreateFCmpUGE(left, right);
                        }
                    }
                    
                    case Token::LEFT_ANGLE: {
                        auto info = get_type_info(bin->left);
                        if (is_int_type(info)) {
                            if (info->is_signed) {
                                return irb->CreateICmpSLT(left, right);
                            } else {
                                return irb->CreateICmpULT(left, right);
                            }
                        } else {
                            assert(is_float_type(info));
                            return irb->CreateFCmpULT(left, right);
                        }
                    }
                    
                    case Token::RIGHT_ANGLE: {
                        auto info = get_type_info(bin->left);
                        if (is_int_type(info)) {
                            if (info->is_signed) {
                                return irb->CreateICmpSGT(left, right);
                            } else {
                                return irb->CreateICmpUGT(left, right);
                            }
                        } else {
                            assert(is_float_type(info));
                            return irb->CreateFCmpUGT(left, right);
                        }
                    }
                    
                    case Token::VERTICAL_BAR: {
                        return irb->CreateOr(left, right);
                    }

                    case Token::AMPERSAND: {
                        return irb->CreateAnd(left, right);
                    }

                    case Token::CARET: {
                        return irb->CreateXor(left, right);
                    }
                    
                    case Token::AND_OP: {
                        assert(left->getType()  == type_i1);
                        assert(right->getType() == type_i1);
                        
                        return irb->CreateAnd(left, right);
                    }
                    case Token::OR_OP: {
                        assert(left->getType()  == type_i1);
                        assert(right->getType() == type_i1);
                        
                        return irb->CreateOr(left, right);
                    }

                    case Token::DEREFERENCE_OR_SHIFT: { // <<
                        // This is the integer binary shift.
                        return irb->CreateShl(left, right);
                    }

                    case Token::RIGHT_SHIFT: {
                        auto info = get_type_info(bin->left);

                        if (info->is_signed) {
                            return irb->CreateAShr(left, right);
                        } else {
                            return irb->CreateLShr(left, right);
                        }
                    }
                    default: assert(false);
                }
            }
            
            assert(false);
        }
        
        case AST_LITERAL: {
            auto lit = static_cast<Ast_Literal *>(expression);
            
            auto type_info = get_type_info(lit);
            auto type = get_type(type_info);

            switch (lit->literal_type) {
                case Ast_Literal::STRING:  return create_string_literal(lit, is_lvalue);

                case Ast_Literal::INTEGER: return ConstantInt::get(type, lit->integer_value, type_info->is_signed);
                case Ast_Literal::FLOAT:   return ConstantFP::get(type,  lit->float_value);
                case Ast_Literal::BOOL:    return ConstantInt::get(type, (lit->bool_value ? 1 : 0));
                case Ast_Literal::NULLPTR: return ConstantPointerNull::get(static_cast<PointerType *>(type));
                default: return nullptr;
            }
        }
        
        case AST_IDENTIFIER: {
            auto ident = static_cast<Ast_Identifier *>(expression);
            assert(ident->resolved_declaration);
            
            if (ident->resolved_declaration->type == AST_DECLARATION) {
                auto decl = static_cast<Ast_Declaration *>(ident->resolved_declaration);
                
                if (decl->is_let && !decl->is_readonly_variable) {
                    return emit_expression(decl->initializer_expression);
                }
                
                if (decl->identifier && compiler->is_toplevel_scope(decl->identifier->enclosing_scope)) {
                    String name = decl->identifier->name->name;
                    auto value = llvm_module->getNamedGlobal(string_ref(name));
                    assert(value);
                    
                    if (!is_lvalue) return irb->CreateLoad(value);
                    return value;
                }
                
                auto value = get_value_for_decl(decl);
                
                if (!is_lvalue) return irb->CreateLoad(value);
                
                return value;
            } else if (ident->resolved_declaration->type == AST_FUNCTION) {
                auto func = static_cast<Ast_Function *>(ident->resolved_declaration);
                
                return get_or_create_function(func);
            } else if (is_a_type_declaration(ident->resolved_declaration)) {
                Ast_Type_Info *type_value = get_type_declaration_resolved_type(ident->resolved_declaration);

                // @Incomplete just stuff the type table index in here for now.. until are able to emit a full type table.
                auto const_int = ConstantInt::get(type_intptr, type_value->type_table_index, true);
                return ConstantExpr::getIntToPtr(const_int, type_i8->getPointerTo());
            } else {
                assert(false);
            }
        }
        
        case AST_DECLARATION: {
            auto decl = static_cast<Ast_Declaration *>(expression);
            auto decl_value = get_value_for_decl(decl);
            
            if (decl->initializer_expression) {
                auto value = emit_expression(decl->initializer_expression);
                irb->CreateStore(value, decl_value);
            } else {
                // if a declaration does not have an initializer, initialize to 0
                auto type_info = get_type_info(decl);
                if (type_info->type == Ast_Type_Info::STRUCT) {
                    default_init_struct(decl_value, type_info);
                } else {
                    auto type = decl_value->getType()->getPointerElementType();
                    irb->CreateStore(Constant::getNullValue(type), decl_value);
                }
            }
            return nullptr;
        }
        
        case AST_FUNCTION_CALL: {
            auto call = static_cast<Ast_Function_Call *>(expression);
            
            // @TODO we should get this naturally from an emit_expression, not from an identifier lookup here.
            auto type_info = get_type_info(call->function_or_function_ptr);
            assert(type_info->type == Ast_Type_Info::FUNCTION);
            
            auto function_target = emit_expression(call->function_or_function_ptr);
            assert(function_target);
            
            bool is_c_function = type_info->is_c_function;
            bool is_win32 = TargetMachine->getTargetTriple().isOSWindows();
            
            Array<Value *> args;
            for (auto &it : call->argument_list) {
                bool is_aggregate_value = is_aggregate_type(get_type_info(it));
                auto value = emit_expression(it, is_aggregate_value);
                
                auto info = get_type_info(it);
                
                if (is_c_function && is_win32 && is_aggregate_type(info)) {
                    assert(info->size >= 0);
                    
                    const int _8BYTES = 8;
                    if (info->size > _8BYTES) {
                        auto alloca = create_alloca_in_entry(irb, get_type(info));
                        
                        auto loaded_value = irb->CreateLoad(value);
                        irb->CreateStore(loaded_value, alloca);
                        args.add(alloca);
                        continue;
                    }
                } else if (is_aggregate_type(info)) {
                    args.add(value);
                    continue;
                }
                
                args.add(value);
            }
            
            // @TODO isnt this incorrect? since we pass in all argument_list items in above?
            
            // promote C vararg arguments if they are not the required sizes
            // for ints, this typically means promoting i8 and i16 to i32.
            if (type_info->is_c_varargs) {
                for (array_count_type i = type_info->arguments.count; i < call->argument_list.count; ++i) {
                    auto value = args[i];
                    auto arg   = call->argument_list[i];
                    
                    auto type = value->getType();
                    if (type->isIntegerTy() && type->getPrimitiveSizeInBits() < type_i32->getPrimitiveSizeInBits()) {
                        assert(is_int_type(get_type_info(arg)) ||
                               get_final_type(get_type_info(arg))->type == Ast_Type_Info::BOOL);
                        if (get_type_info(arg)->is_signed) {
                            args[i] = irb->CreateSExt(value, type_i32);
                        } else {
                            args[i] = irb->CreateZExt(value, type_i32);
                        }
                    } else if (type->isFloatTy() && type->getPrimitiveSizeInBits() < type_f64->getPrimitiveSizeInBits()) {
                        assert(is_float_type(get_type_info(arg)));
                        args[i] = irb->CreateFPExt(value, type_f64);
                    }
                }
            }
            
            Value *result = irb->CreateCall(function_target, ArrayRef<Value *>(args.data, args.count));
            
            if (is_lvalue) {
                auto alloca = create_alloca_in_entry(irb, result->getType());
                irb->CreateStore(result, alloca);
                return alloca;
            }
            
            return result;
        }
        
        case AST_DEREFERENCE: {
            auto deref = static_cast<Ast_Dereference *>(expression);
            
            auto lhs = emit_expression(deref->left, true);
            
            assert(deref->element_path_index >= 0);
            
            // @Incomplete
            // assert(deref->byte_offset >= 0);
            
            auto value = dereference(lhs, deref->element_path_index, is_lvalue);
            return value;
        }
        
        case AST_CAST: {
            auto cast = static_cast<Ast_Cast *>(expression);
            Value *value = emit_expression(cast->expression);
            
            auto src = get_type_info(cast->expression);
            auto dst = cast->type_info;
            
            auto src_type = get_type(src);
            auto dst_type = get_type(dst);
            if (is_int_type(src) && is_int_type(dst)) {
                if (src->size > dst->size) {
                    return irb->CreateTrunc(value, dst_type);
                } else if (src->size < dst->size) {
                    if (src->is_signed && dst->is_signed) {
                        return irb->CreateSExt(value, dst_type);
                    } else {
                        return irb->CreateZExt(value, dst_type);
                    }
                }
                
                assert(src_type == dst_type);
                return value;
            } else if (is_float_type(src) && is_float_type(dst)) {
                if (src->size < dst->size) {
                    return irb->CreateFPExt(value, dst_type);
                } else if (src->size > dst->size) {
                    return irb->CreateFPTrunc(value, dst_type);
                }
                
                assert(src_type == dst_type);
                return value;
            } else if (is_float_type(src) && is_int_type(dst)) {
                if (dst->is_signed) {
                    return irb->CreateFPToSI(value, dst_type);
                } else {
                    return irb->CreateFPToUI(value, dst_type);
                }
            } else if (is_int_type(src) && is_float_type(dst)) {
                if (src->is_signed) {
                    return irb->CreateSIToFP(value, dst_type);
                } else {
                    return irb->CreateUIToFP(value, dst_type);
                }
            } else if (is_pointer_type(src) && is_pointer_type(dst)) {
                return irb->CreatePointerCast(value, dst_type);
            } else if (is_pointer_type(src) && is_int_type(dst)) {
                return irb->CreatePtrToInt(value, dst_type);
            } else if (is_int_type(src) && is_pointer_type(dst)) {
                return irb->CreateIntToPtr(value, dst_type);
            } else if (is_pointer_type(src) && dst->type == Ast_Type_Info::FUNCTION) {
                return irb->CreatePointerCast(value, dst_type);
            }
            
            assert(false);
            break;
        }
        
        case AST_IF: {
            auto _if = static_cast<Ast_If *>(expression);
            auto cond = emit_expression(_if->condition);
            
            auto current_block = irb->GetInsertBlock();
            
            BasicBlock *next_block = BasicBlock::Create(*llvm_context, "", current_block->getParent());
            BasicBlock *then_block = nullptr;
            BasicBlock *else_block = nullptr;
            
            BasicBlock *failure_target = next_block;
            
            then_block = BasicBlock::Create(*llvm_context, "then_target", current_block->getParent());
            if (_if->then_statement) {
                irb->SetInsertPoint(then_block);
                emit_expression(_if->then_statement);
                
            }
            if (!irb->GetInsertBlock()->getTerminator()) irb->CreateBr(next_block);
            
            if (_if->else_statement) {
                else_block = BasicBlock::Create(*llvm_context, "else_target", current_block->getParent());
                irb->SetInsertPoint(else_block);
                emit_expression(_if->else_statement);
                
                if (!irb->GetInsertBlock()->getTerminator()) irb->CreateBr(next_block);
                
                failure_target = else_block;
            }
            
            irb->SetInsertPoint(current_block);
            irb->CreateCondBr(cond, then_block, failure_target);
            irb->SetInsertPoint(next_block);
            
            break;
        }
        
        case AST_WHILE: {
            auto loop = static_cast<Ast_While *>(expression);
            
            auto current_block = irb->GetInsertBlock();
            
            BasicBlock *next_block = BasicBlock::Create(*llvm_context, "loop_exit", current_block->getParent());
            BasicBlock *loop_header = BasicBlock::Create(*llvm_context, "loop_header", current_block->getParent());
            BasicBlock *loop_body = BasicBlock::Create(*llvm_context, "loop_body", current_block->getParent());

            loop_header_map.add(MakeTuple(static_cast<Ast_Expression *>(loop), loop_header));
            loop_exit_map.add(MakeTuple(static_cast<Ast_Expression *>(loop), next_block));
            
            
            irb->CreateBr(loop_header);
            
            irb->SetInsertPoint(loop_header);
            // emit the condition in the loop header so that it always executes when we loop back around
            auto cond = emit_expression(loop->condition);
            irb->SetInsertPoint(loop_header);
            irb->CreateCondBr(cond, loop_body, next_block);
            
            irb->SetInsertPoint(loop_body);
            if (loop->statement) {
                emit_expression(loop->statement);
                // irb->SetInsertPoint(loop_body);
                if (!irb->GetInsertBlock()->getTerminator()) irb->CreateBr(loop_header);
            }
            
            irb->SetInsertPoint(next_block);
            break;
        }
        
        case AST_FOR: {
            auto _for = static_cast<Ast_For *>(expression);
            
            auto it_decl = _for->iterator_decl;
            auto it_alloca = create_alloca_in_entry(irb, get_type(get_type_info(it_decl)));
            auto decl_type = get_type_info(it_decl);
            
            decl_value_map.add(MakeTuple(it_decl, it_alloca));
            
            auto it_index_decl = _for->iterator_index_decl;
            Ast_Type_Info *it_index_type = nullptr;
            Value *it_index_alloca = nullptr;
            if (it_index_decl) {
                it_index_type = get_type_info(it_index_decl);
                it_index_alloca = create_alloca_in_entry(irb, get_type(it_index_type));
                decl_value_map.add(MakeTuple(it_index_decl, it_index_alloca));
                emit_expression(it_index_decl);
            } else {
                it_index_type = decl_type;
                it_index_alloca = it_alloca;
                
                emit_expression(it_decl);
            }
            
            auto current_block = irb->GetInsertBlock();
            
            BasicBlock *next_block = BasicBlock::Create(*llvm_context, "for_exit", current_block->getParent());
            BasicBlock *loop_header = BasicBlock::Create(*llvm_context, "for_header", current_block->getParent());
            BasicBlock *loop_body = BasicBlock::Create(*llvm_context, "for_body", current_block->getParent());

            // Where the iterator increment happens. This is factored out to be the target of _continue_ statements.
            BasicBlock *loop_body_end = BasicBlock::Create(*llvm_context, "for_body_end", current_block->getParent());

            loop_header_map.add(MakeTuple(static_cast<Ast_Expression *>(_for), loop_body_end));
            loop_exit_map.add(MakeTuple(static_cast<Ast_Expression *>(_for), next_block));
            
            
            irb->CreateBr(loop_header);
            
            irb->SetInsertPoint(loop_header);
            // emit the condition in the loop header so that it always executes when we loop back around
            auto it_index = irb->CreateLoad(it_index_alloca);
            assert(is_int_type(it_index_type));
            
            auto upper = emit_expression(_for->upper_range_expression);
            Value *cond = nullptr;
            if (it_index_decl || _for->is_exclusive_end) {
                // use < here otherwise, we'll overstep by one.
                // @Cleanup maybe this should be flagged as a half-open loop
                // when we support that?
                if (it_index_type->is_signed) {
                    cond = irb->CreateICmpSLT(it_index, upper);
                } else {
                    cond = irb->CreateICmpULT(it_index, upper);
                }
            } else {
                if (it_index_type->is_signed) {
                    cond = irb->CreateICmpSLE(it_index, upper);
                } else {
                    cond = irb->CreateICmpULE(it_index, upper);
                }
            }
            
            irb->SetInsertPoint(loop_header);
            irb->CreateCondBr(cond, loop_body, next_block);
            
            irb->SetInsertPoint(loop_body);
            if (it_index_decl) {
                emit_expression(it_decl);
            }
            
            emit_scope(&_for->body);
            
            if (!irb->GetInsertBlock()->getTerminator()) irb->CreateBr(loop_body_end);

            irb->SetInsertPoint(loop_body_end);
            irb->CreateStore(irb->CreateAdd(it_index, ConstantInt::get(get_type(it_index_type), 1)), it_index_alloca);
            irb->CreateBr(loop_header);
            
            irb->SetInsertPoint(next_block);
            break;
        }
        
        case AST_RETURN: {
            auto ret = static_cast<Ast_Return *>(expression);
            if (ret->expression) {
                auto value = emit_expression(ret->expression);
                assert(value);
                irb->CreateRet(value);
            } else {
                irb->CreateRetVoid();
            }
            
            // Create a new block so subsequent instructions have some where to generate to
            // @TODO Actually, Idk if this is correct, will have to test with how ifs and loops work...
            
            /*
            auto current_block = irb->GetInsertBlock();
            BasicBlock *new_block = BasicBlock::Create(*llvm_context, "", current_block->getParent());
            
            irb->SetInsertPoint(new_block);
            */
            break;
        }
        
        case AST_ARRAY_DEREFERENCE: {
            auto deref = static_cast<Ast_Array_Dereference *>(expression);
            
            auto array = emit_expression(deref->array_or_pointer_expression, true);
            auto index = emit_expression(deref->index_expression);
            
            auto type = get_type_info(deref->array_or_pointer_expression);
            type = get_final_type(type);

            if (type->type == Ast_Type_Info::ARRAY && type->array_element_count == -1) {
                // @Cleanup hardcoded indices
                array = irb->CreateGEP(array, {ConstantInt::get(type_i32, 0), ConstantInt::get(type_i32, 0)});
                array = irb->CreateLoad(array);
                auto element = irb->CreateGEP(array, index);
                
                if (!is_lvalue) return irb->CreateLoad(element);
                return element;
            } else if (type->type == Ast_Type_Info::STRING) {
                // @Note although this is identical to the dynamic/static array case,
                // I've chosen to duplicate the code in case we chnage the order of
                // any of these implicit struct fields.
                // @Cleanup hardcoded indices.
                array = irb->CreateGEP(array, {ConstantInt::get(type_i32, 0), ConstantInt::get(type_i32, 0)});
                array = irb->CreateLoad(array);
                auto element = irb->CreateGEP(array, index);
                
                if (!is_lvalue) return irb->CreateLoad(element);
                return element;
            } else if (type->type == Ast_Type_Info::POINTER) {
                auto ptr = irb->CreateLoad(array);
                auto element = irb->CreateGEP(ptr, {index});
                
                if (!is_lvalue) return irb->CreateLoad(element);
                return element;
            }
            
            // @Cleanup type_i32 use for array indexing
            auto element = irb->CreateGEP(array, {ConstantInt::get(type_i32, 0), index});
            
            if (!is_lvalue) return irb->CreateLoad(element);
            return element;
        }
        
        case AST_FUNCTION: {
            auto func = static_cast<Ast_Function *>(expression);
            
            if (func->is_template_function) {
                // we should not get here if this was an expression use of a function
                return nullptr;
            }
            
            // we only need the header to be generated when we come here, only the compiler instance can choose to emit a function.
            return get_or_create_function(func);
        }

        case AST_CONTROL_FLOW: {
            auto flow = static_cast<Ast_Control_Flow *>(expression);

            if (flow->control_type == Token::KEYWORD_BREAK) {
                for (auto &entry : loop_exit_map) {
                    if (entry.item1 == flow->target_statement) return irb->CreateBr(entry.item2);
                }
            } else if (flow->control_type == Token::KEYWORD_CONTINUE) {
                for (auto &entry : loop_header_map) {
                    if (entry.item1 == flow->target_statement) return irb->CreateBr(entry.item2);
                }
            }

            assert(false && "Could not find LLVM BasicBlock for control-flow statement.");
            return nullptr;
        }
    }
    
    return nullptr;
}

Function *LLVM_Generator::get_or_create_function(Ast_Function *function) {
    assert(function->identifier);
    String linkage_name = function->linkage_name;
    
    auto func = llvm_module->getFunction(string_ref(linkage_name));
    
    if (!func) {
        FunctionType *function_type = create_function_type(function);
        func = Function::Create(function_type, GlobalValue::LinkageTypes::ExternalLinkage, string_ref(linkage_name), llvm_module);
        
        array_count_type i = 0;
        for (auto &a : func->args()) {
            if (i < function->arguments.count) {
                a.setName(string_ref(function->arguments[i]->identifier->name->name));
                
                ++i;
            }
        }
    }
    
    return func;
}

void LLVM_Generator::emit_scope(Ast_Scope *scope) {
    auto old_di_scope = di_current_scope;
    di_current_scope = dib->createLexicalBlock(old_di_scope, get_debug_file(llvm_context, scope), get_line_number(scope), 0);

    auto current_block = irb->GetInsertBlock();
    auto func = current_block->getParent();
    BasicBlock *entry_block = &func->getEntryBlock();

    // setup variable mappings
    for (auto it : scope->declarations) {
        while (it->substitution) it = it->substitution;
        
        if (it->type != AST_DECLARATION) continue;
        auto decl = static_cast<Ast_Declaration *>(it);
        
        auto alloca = create_alloca_in_entry(irb, get_type(get_type_info(it)));
        
        String name;
        if (decl->identifier) name = decl->identifier->name->name;

        if (decl->identifier) {
            alloca->setName(string_ref(name));
        }
        
        assert(get_value_for_decl(decl) == nullptr);
        decl_value_map.add(MakeTuple(decl, alloca));

        // debug info

        // @TODO this should be based on desired optimization. Though, in my experience,
        // even this flag doesn't help preserve the actual stack variable much on Windows
        // without using a hack like inserting a GEP. -josh 18 August 2019
        bool always_preserve = true;
        auto di_type = get_debug_type(get_type_info(it));
        auto di_local_var = dib->createAutoVariable(di_current_scope, string_ref(name),
                                get_debug_file(llvm_context, it), get_line_number(it), di_type, always_preserve);
        auto declare = dib->insertDeclare(alloca, di_local_var, DIExpression::get(*llvm_context, None), DebugLoc::get(get_line_number(it), 0, di_current_scope),
                            entry_block);
    }

    // @Cleanup private decclarations should just be denoted by a flag.
    for (auto it : scope->private_declarations) {
        while (it->substitution) it = it->substitution;
        
        if (it->type != AST_DECLARATION) continue;
        auto decl = static_cast<Ast_Declaration *>(it);
        
        auto alloca = create_alloca_in_entry(irb, get_type(get_type_info(it)));
        
        String name;
        if (decl->identifier) name = decl->identifier->name->name;

        if (decl->identifier) {
            alloca->setName(string_ref(name));
        }
        
        assert(get_value_for_decl(decl) == nullptr);
        decl_value_map.add(MakeTuple(decl, alloca));

        // debug info
        bool always_preserve = true;
        auto di_type = get_debug_type(get_type_info(it));
        auto di_local_var = dib->createAutoVariable(di_current_scope, string_ref(name),
                                get_debug_file(llvm_context, it), get_line_number(it), di_type, always_preserve);
        dib->insertDeclare(alloca, di_local_var, DIExpression::get(*llvm_context, None), DebugLoc::get(get_line_number(it), 0, di_current_scope),
                            entry_block);
    }
    
    for (auto &it : scope->statements) {
        irb->SetCurrentDebugLocation(DebugLoc::get(get_line_number(it), 0, di_current_scope));
        emit_expression(it);
    }

    di_current_scope = old_di_scope;
}

DISubroutineType *LLVM_Generator::get_debug_subroutine_type(Ast_Type_Info *type) {
    Array<Metadata *> arguments;


    DIType *return_type = get_debug_type(type->return_type);
    // @Incomplete void return types need to be null?

    arguments.add(return_type);
        
    bool is_c_function = type->is_c_function;
    bool is_win32 = TargetMachine->getTargetTriple().isOSWindows();
    
    for (auto arg_type : type->arguments) {
        if (arg_type == compiler->type_void) continue;
        
        DIType *di_type = get_debug_type(arg_type);
        
        // if (is_c_function && is_win32 && is_aggregate_type(arg_type)) {
        //     assert(arg_type->size >= 0);
            
        //     // @TargetInfo this is only true for x64 too
        //     const int _8BYTES = 8;
        //     if (arg_type->size > _8BYTES) {
        //         arguments.add(type->getPointerTo());
        //         continue;
        //     }
        // }

        if (is_aggregate_type(arg_type)) {
            di_type = dib->createReferenceType(dwarf::DW_TAG_reference_type, di_type, TargetMachine->getPointerSizeInBits(0));
        }
        
        arguments.add(di_type);
    }

    return dib->createSubroutineType(dib->getOrCreateTypeArray(ArrayRef<Metadata *>(arguments.data, arguments.count)));
}

void LLVM_Generator::emit_function(Ast_Function *function) {
    assert(function->identifier && function->identifier->name);
    
    Function *func = get_or_create_function(function);

    if (!function->scope) return; // forward declaration of external thing

    if (!func->empty()) {
        String name = function->linkage_name;
        compiler->report_error(function, "Function with linkage name \"%.*s\" already has been defined!\n", name.length, name.data);
        return;
    }

    StringRef function_name = string_ref(function->identifier->name->name);
    StringRef linkage_name  = string_ref(function->linkage_name);
    auto subroutine_type    = get_debug_subroutine_type(get_type_info(function));
    assert(di_current_scope);
    auto di_subprogram      = dib->createFunction(di_current_scope, function_name, linkage_name,
                                            get_debug_file(llvm_context, function), get_line_number(function),
                                            subroutine_type, get_line_number(function->scope), DINode::FlagPrototyped, DISubprogram::SPFlagDefinition);
    func->setSubprogram(di_subprogram);

    auto old_di_scope = di_current_scope;
    di_current_scope = di_subprogram;

    irb->SetCurrentDebugLocation(DebugLoc());

    // create entry block
    BasicBlock *entry = BasicBlock::Create(*llvm_context, "entry", func);
    BasicBlock *starting_block = BasicBlock::Create(*llvm_context, "start", func);
    
    
    irb->SetInsertPoint(entry);
    
    auto arg_it = func->arg_begin();
    for (array_count_type i = 0; i < function->arguments.count; ++i) {
        auto a  = arg_it;
        
        auto decl = function->arguments[i];

        Value *storage = nullptr;
        if (is_aggregate_type(get_type_info(decl))) {
            // Aggregate parameters are already references/pointers so we don't need storage for them.
            assert(get_value_for_decl(decl) == nullptr);
            decl_value_map.add(MakeTuple(decl, static_cast<Value *>(a)));

            storage = a;
        } else {
            // Create storage for value-parameters so that we can treat the parameters
            // the same as local variables during code generation. Maybe this isn't super necessary anymore, idk!
            Value *alloca = irb->CreateAlloca(get_type(get_type_info(decl)));
            irb->CreateStore(a, alloca);
            
            assert(get_value_for_decl(decl) == nullptr);
            decl_value_map.add(MakeTuple(decl, alloca));

            storage = alloca;
        }

        String name;
        if (decl->identifier) name = decl->identifier->name->name;

        auto di_type = get_debug_type(get_type_info(decl));
        bool always_preserve = true; // @TODO should be based on desired optimization.
        auto param = dib->createParameterVariable(di_subprogram, string_ref(name), i+1,
                            get_debug_file(llvm_context, decl), get_line_number(decl),
                            di_type, always_preserve);
        auto declare = dib->insertDeclare(storage, param, DIExpression::get(*llvm_context, None), DebugLoc::get(get_line_number(decl), 0, di_subprogram),
                            starting_block);

        arg_it++;
    }
    
    irb->CreateBr(starting_block);
    
    irb->SetInsertPoint(starting_block);
    emit_scope(function->scope);
    
    auto current_block = irb->GetInsertBlock();
    
    if (!current_block->getTerminator()) {
        auto return_decl = function->return_decl;
        // @Cleanup early out for void types since we use i8 for pointers
        if (return_decl && get_type_info(return_decl)->type != Ast_Type_Info::VOID) {
            irb->CreateRet(Constant::getNullValue(get_type(get_type_info(return_decl))));
        } else {
            irb->CreateRetVoid();
        }
    }

    decl_value_map.clear();
    loop_header_map.clear();
    loop_exit_map.clear();

    di_current_scope = old_di_scope;
}

void LLVM_Generator::emit_global_variable(Ast_Declaration *decl) {
    bool is_constant = false;
    String name = decl->identifier->name->name;
    Type *type = get_type(get_type_info(decl));
    
    Constant *const_init = nullptr;
    if (decl->initializer_expression) {
        auto init = emit_expression(decl->initializer_expression);
        const_init = dyn_cast<llvm::Constant>(init);
        assert(const_init);
    } else {
        const_init = Constant::getNullValue(type);
    }
    
    auto GV = new GlobalVariable(*llvm_module, type, is_constant, GlobalVariable::InternalLinkage, const_init, string_ref(name));
    
    // printf("EMIT GV: '%.*s'\n", name.length, name.data);
}

#ifdef WIN32
#define PATH_SEPARATOR "\\"
#else
#define PATH_SEPARATOR "/"
#endif

#include <stdio.h>

void LLVM_Jitter::init() {

    for (auto lib: compiler->libraries) {
        String name = lib->libname;
        auto c_str = to_c_string(name);
        if (!lib->is_framework) {
            bool not_valid = llvm::sys::DynamicLibrary::LoadLibraryPermanently(c_str);
            if (not_valid) { // It cannot be loaded by name alone so it may not be a system library. Try provided search paths instead.
                for (auto path: compiler->library_search_paths) {

                    String fullpath;
                    if (llvm->TargetMachine->getTargetTriple().isOSWindows()) {
                        fullpath = mprintf("%.*s" PATH_SEPARATOR "%s", path.length, path.data, c_str);
                    } else {
                        char *ext = "so";
                        if (llvm->TargetMachine->getTargetTriple().isMacOSX()) ext = "dylib";

                        fullpath = mprintf("%.*s" PATH_SEPARATOR "lib%s.%s", path.length, path.data, c_str, ext);
                    }
                    auto fullpath_c_string = to_c_string(fullpath);
                    // printf("PATH: %s\n", fullpath_c_string);
                    not_valid = llvm::sys::DynamicLibrary::LoadLibraryPermanently(fullpath_c_string);
                    free(fullpath.data);
                    free(fullpath_c_string);
                    if (!not_valid) break;
                }
            }
        } else {
            auto fullpath = mprintf("/System/Library/Frameworks" PATH_SEPARATOR "%s.framework" PATH_SEPARATOR "%s", c_str, c_str);
            auto fullpath_c_string = to_c_string(fullpath);
            // printf("PATH: %s\n", fullpath_c_string);
            bool not_valid = llvm::sys::DynamicLibrary::LoadLibraryPermanently(fullpath_c_string);
            free(fullpath.data);
            free(fullpath_c_string);
            // if (!not_valid) break;
        }

        free(c_str);
    }

    auto JTMB = JITTargetMachineBuilder::detectHost();
    
    if (!JTMB) {
        JTMB.takeError();
        return;
    }
    
    auto DL = JTMB->getDefaultDataLayoutForTarget();
    if (!DL) {
        DL.takeError();
        return;
    }

    ExecutionSession ES;
    RTDyldObjectLinkingLayer ObjectLayer(ES, []() { return llvm::make_unique<SectionMemoryManager>(); });
    
#ifdef WIN32
    ObjectLayer.setOverrideObjectFlagsWithResponsibilityFlags(true);

    // Sigh, I dont know why but setting this works around an LLVM bug that trips this assert on Windows:
    // "Resolving symbol outside this responsibility set"
    // There's a Stack Overflow thread discussing the issue here:
    // https://stackoverflow.com/questions/57733912/llvm-asserts-resolving-symbol-outside-this-responsibility-set#comment101934807_57733912
    // For now, the jiyu-game project runs as a metaprogram with this flag set. 
    // -josh 18 November 2019
    ObjectLayer.setAutoClaimResponsibilityForObjectSymbols(true);
#endif
    
    IRCompileLayer CompileLayer(ES, ObjectLayer, ConcurrentIRCompiler(*JTMB));
    
    MangleAndInterner Mangle(ES, *DL);
    
    ES.getMainJITDylib().setGenerator(cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(*DL)));
    
    llvm->llvm_module->setDataLayout(*DL);
    // llvm->llvm_module->dump();
    
    if (!llvm->llvm_module->getFunction("main")) {
        compiler->report_error((Token *)nullptr, "No main function defined for meta program. Aborting.\n");
        return;
    }
    
    cantFail(CompileLayer.add(ES.getMainJITDylib(),
                              ThreadSafeModule(std::unique_ptr<Module>(llvm->llvm_module), *llvm->thread_safe_context)));
    
    auto sym = ES.lookup({&ES.getMainJITDylib()}, Mangle("main"));
    if (!sym) {
        sym.takeError();
        return;
    }
    
    auto *Main = (void (*)(s32 argc, char **argv)) sym->getAddress();
    Main(compiler->metaprogram_argc, compiler->metaprogram_argv);
}

void *LLVM_Jitter::lookup_symbol(String name) {
    return nullptr;
}