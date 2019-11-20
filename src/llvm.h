
#ifndef LLVM_H
#define LLVM_H

#include "general.h"

namespace llvm {
    class Module;
    class LLVMContext;
    
    class Value;
    
    class Type;
    class FunctionType;
    class StructType;
    class Function;
    class BasicBlock;

    class TargetMachine;
    class ConstantFolder;
    class IRBuilderDefaultInserter;
    class DIBuilder;
    class DICompileUnit;
    class DIType;
    class DISubroutineType;
    class DIScope;
    class DILexicalBlock;
    
    template<typename T, typename Inserter> class IRBuilder;
    
    namespace orc {
        class ExecutionSession;
        class RTDyldObjectLinkingLayer;
        class IRCompileLayer;
        class ThreadSafeContext;
        class MangleAndInterner;
    };
    
};

struct Compiler;
struct Ast_Function;
struct Ast_Type_Info;
struct Ast_Declaration;
struct Ast_Scope;
struct Ast_Expression;
struct Ast_Literal;

struct LLVM_Generator {
    Compiler *compiler;
    
    llvm::TargetMachine *TargetMachine;
    
    String obj_output_name;
    llvm::Module  *llvm_module;
    llvm::LLVMContext *llvm_context;
    llvm::orc::ThreadSafeContext *thread_safe_context;
    llvm::IRBuilder<llvm::ConstantFolder, llvm::IRBuilderDefaultInserter> *irb;
    
    llvm::DIBuilder *dib;
    llvm::DICompileUnit *di_compile_unit;
    
    llvm::Type *type_void;
    llvm::Type *type_i1;
    llvm::Type *type_i8;
    llvm::Type *type_i16;
    llvm::Type *type_i32;
    llvm::Type *type_i64;
    llvm::Type *type_f32;
    llvm::Type *type_f64;
    
    llvm::StructType *type_string;
    llvm::Type *type_string_length;
    
    llvm::Type *type_intptr;

    // Debug types
    llvm::DIType *di_type_bool;
    llvm::DIType *di_type_s8;
    llvm::DIType *di_type_s16;
    llvm::DIType *di_type_s32;
    llvm::DIType *di_type_s64;
    llvm::DIType *di_type_u8;
    llvm::DIType *di_type_u16;
    llvm::DIType *di_type_u32;
    llvm::DIType *di_type_u64;
    llvm::DIType *di_type_f32;
    llvm::DIType *di_type_f64;

    llvm::DIType *di_type_string;
    llvm::DIType *di_type_string_length;
    llvm::DIType *di_type_type;
    
    Array<Tuple<Ast_Declaration *, llvm::Value *>>     decl_value_map;

    // Used for looking up the target BasicBlock for _continue_ and _break_. We probably want something more robust than this later on.
    Array<Tuple<Ast_Expression *, llvm::BasicBlock *>> loop_header_map; // Jump target for _continue_
    Array<Tuple<Ast_Expression *, llvm::BasicBlock *>> loop_exit_map;    // Jump target for _break_

    llvm::DIScope *di_current_scope = nullptr;

    Array<llvm::Type *> llvm_types;
    Array<llvm::DIType *> llvm_debug_types;
    
    
    LLVM_Generator(Compiler *compiler) {
        this->compiler = compiler;
    }
    
    void preinit();
    void init();
    void finalize();
    
    llvm::Value *create_string_literal(Ast_Literal *lit, bool want_lvalue = false);
    llvm::Value *get_value_for_decl(Ast_Declaration *decl);
    llvm::Value *dereference(llvm::Value *value, s64 element_path_index, bool is_lvalue = false);
    void default_init_struct(llvm::Value *decl_value, Ast_Type_Info *info);
    
    llvm::Function *get_or_create_function(Ast_Function *function);
    llvm::Type *get_type(Ast_Type_Info *type);
    llvm::Type *make_llvm_type(Ast_Type_Info *type);
    llvm::FunctionType *create_function_type(Ast_Function *function);
    void emit_scope(Ast_Scope *scope);
    void emit_function(Ast_Function *function);
    void emit_global_variable(Ast_Declaration *decl);
    llvm::Value *emit_expression(Ast_Expression *expression, bool is_lvalue = false);

    llvm::DISubroutineType *get_debug_subroutine_type(Ast_Type_Info *type);
    llvm::DIType           *get_debug_type(Ast_Type_Info *type);
};

struct LLVM_Jitter {
    /*
    llvm::orc::ExecutionSession         *execution_session;
    llvm::orc::RTDyldObjectLinkingLayer *object_layer;
    llvm::orc::IRCompileLayer           *compile_layer;
    llvm::orc::MangleAndInterner        *mangler;
    */
    
    Compiler *compiler;
    LLVM_Generator *llvm;
    
    LLVM_Jitter(LLVM_Generator *_llvm) {
        this->llvm     = _llvm;
        this->compiler = _llvm->compiler;
    }
    
    void init();
    void *lookup_symbol(String name);
};


#endif
