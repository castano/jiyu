#include "clang_import.h"
#include "compiler.h"

#include <new> // for placement new
#include <clang-c/Index.h>

#define IMPORT_NEW(type) (new (compiler->get_memory(sizeof(type))) type())

// Map Clang USR to Jiyu AST nodes.
struct USR_Pair {
    String usr;
    Ast   *ast;
};

Ast *find_ast(Array<USR_Pair> *map, String usr) {
    for (auto entry: (*map)) {
        if (entry.usr == usr) {
            return entry.ast;
        }
    }

    return nullptr;
}

void add_usr_mapping(Array<USR_Pair> *map, String usr, Ast *jiyu_ast) {
    assert(find_ast(map, usr) == nullptr);

    USR_Pair pair;
    pair.usr = usr;
    pair.ast = jiyu_ast;

    map->add(pair);
}

struct Visitor_Data {
    Compiler *compiler;
    Ast_Scope *target_scope;
    Array<USR_Pair>    *usr_map = nullptr;
};

static
String copy_and_dispose(Compiler *compiler, CXString input) {
    String result = compiler->copy_string(to_string(clang_getCString(input)));
    clang_disposeString(input);
    return result;
}

static
Ast_Type_Info *get_jiyu_type(Visitor_Data *data, CXType type) {
    Compiler *compiler = data->compiler;
    CXTypeKind kind = type.kind;

    switch (kind) {
        /* Builtin types */
        case CXType_Void:   return compiler->type_void;
        case CXType_Bool:   return compiler->type_bool;

        case CXType_Char16: return compiler->type_uint16;
        case CXType_Char32: return compiler->type_uint32;
        
        case CXType_Char_U:
        case CXType_UChar:
        case CXType_UShort:
        case CXType_UInt:
        case CXType_ULong:
        case CXType_ULongLong:
        case CXType_UInt128: {
            auto size = clang_Type_getSizeOf(type);

            if      (size == 1) return compiler->type_uint8;
            else if (size == 2) return compiler->type_uint16;
            else if (size == 4) return compiler->type_uint32;
            else if (size == 8) return compiler->type_uint64;
            else assert(false); // @Incomplete 128bit

            return nullptr;
        }


        case CXType_Char_S:
        case CXType_SChar:
        case CXType_WChar:
        case CXType_Short:
        case CXType_Int:
        case CXType_Long:
        case CXType_LongLong:
        case CXType_Int128: {
            auto size = clang_Type_getSizeOf(type);

            if      (size == 1) return compiler->type_int8;
            else if (size == 2) return compiler->type_int16;
            else if (size == 4) return compiler->type_int32;
            else if (size == 8) return compiler->type_int64;
            else assert(false); // @Incomplete 128bit

            return nullptr;
        }

        case CXType_Float16:
        case CXType_Half:
        case CXType_Float:
        case CXType_Double:
        case CXType_LongDouble:
        case CXType_Float128: {
            auto size = clang_Type_getSizeOf(type);

            if      (size == 4) return compiler->type_float32;
            else if (size == 8) return compiler->type_float64;
            else assert(false); // @Incomplete 128bit ? half/float16

            return nullptr;
        }


        // CXType_NullPtr = 24,
        // CXType_Overload = 25,
        // CXType_Dependent = 26,
        // CXType_ObjCId = 27,
        // CXType_ObjCClass = 28,
        // CXType_ObjCSel = 29,
        // CXType_ShortAccum = 33,
        // CXType_Accum = 34,
        // CXType_LongAccum = 35,
        // CXType_UShortAccum = 36,
        // CXType_UAccum = 37,
        // CXType_ULongAccum = 38,
        // CXType_FirstBuiltin = CXType_Void,
        // CXType_LastBuiltin = CXType_ULongAccum,

        // CXType_Complex = 100,

        case CXType_Pointer: {
            CXType pointee = clang_getPointeeType(type);
            return compiler->make_pointer_type(get_jiyu_type(data, pointee));
        }
        
        // CXType_BlockPointer = 102,
        // CXType_LValueReference = 103,
        // CXType_RValueReference = 104,
        case CXType_Record: {
            CXCursor struct_decl = clang_getTypeDeclaration(type);
            struct_decl = clang_getCanonicalCursor(struct_decl);
            String usr = copy_and_dispose(compiler, clang_getCursorUSR(struct_decl));
            Ast *ast = find_ast(data->usr_map, usr);

            assert(ast && ast->type == AST_STRUCT);
            auto result = get_type_declaration_resolved_type(ast);
            assert(result);
            return result;
        }
        
        // @Incomplete enums arent supported in jiyu yet
        case CXType_Enum: {
            CXCursor type_decl = clang_getTypeDeclaration(type);
            String usr = copy_and_dispose(compiler, clang_getCursorUSR(type_decl));
            Ast *ast = find_ast(data->usr_map, usr);

            assert(ast && ast->type == AST_TYPE_ALIAS); // @Incomplete change to AST_ENUM when that exists.
            auto result = get_type_declaration_resolved_type(ast);
            assert(result);
            return result;
        }
        

        case CXType_Typedef: {
            CXCursor type_decl = clang_getTypeDeclaration(type);
            String usr = copy_and_dispose(compiler, clang_getCursorUSR(type_decl));
            Ast *ast = find_ast(data->usr_map, usr);

            assert(ast && ast->type == AST_TYPE_ALIAS);
            auto result = get_type_declaration_resolved_type(ast);
            assert(result);
            return result;
        }

        // CXType_ObjCInterface = 108,
        // CXType_ObjCObjectPointer = 109,
        // CXType_FunctionNoProto = 110,
        case CXType_FunctionProto: {
            // @Cutnpaste from Compiler::make_function_type
            Ast_Type_Info *info = IMPORT_NEW(Ast_Type_Info);
            info->type   = Ast_Type_Info::FUNCTION;
            info->size   = compiler->type_ptr_void->size;
            info->stride = compiler->type_ptr_void->stride;
            
            info->is_c_function = true;
            info->is_c_varargs  = (clang_isFunctionTypeVariadic(type) != 0);
            
            auto arg_count = clang_getNumArgTypes(type);
            for (int i = 0; i < arg_count; ++i) {
                auto arg_info = get_jiyu_type(data, clang_getArgType(type, i));
                
                info->arguments.add(arg_info);
            }
            
            
            info->return_type = get_jiyu_type(data, clang_getResultType(type));
            
            compiler->add_to_type_table(info);
            return info;
        }
        case CXType_ConstantArray: {
            auto element_count = clang_getNumElements(type);
            auto element_type = get_jiyu_type(data, clang_getArrayElementType(type));

            return compiler->make_array_type(element_type, element_count, /*is_dynamic=*/false);
        }
        // CXType_Vector = 113,

        // @Incomplete array types
        case CXType_IncompleteArray: {
            // <type> name[]; just turn these into pointers since we dont have compatible types
            // and a pointer allows the user to just do: <jiyu_array>.data.
            auto element_type = get_jiyu_type(data, clang_getArrayElementType(type));
            return compiler->make_pointer_type(element_type);
        }
        // CXType_VariableArray = 115,
        // CXType_DependentSizedArray = 116,

        // CXType_MemberPointer = 117,
        // CXType_Auto = 118,

        case CXType_Elaborated: { // Why Clang, why.
            return get_jiyu_type(data, clang_Type_getNamedType(type));
        }
    }

    CXString kind_string = clang_getTypeKindSpelling(type.kind);
    compiler->report_error((Ast *)nullptr, "Unhandled CXTypeKind in get_jiyu_type: %s. This is a message for maintainers.\n", clang_getCString(kind_string));
    clang_disposeString(kind_string);

    assert(false);
    return nullptr;
}

static
CXChildVisitResult cursor_visitor(CXCursor cursor, CXCursor parent, CXClientData client_data) {
    Visitor_Data *visitor_data = reinterpret_cast<Visitor_Data *>(client_data);
    Compiler  *compiler       = visitor_data->compiler;
    Ast_Scope *current_scope = visitor_data->target_scope;
    Array<USR_Pair>    *usr_map = visitor_data->usr_map;

    auto kind_string = clang_getCursorKindSpelling(cursor.kind);
    // printf("USR   %s\n", clang_getCString(clang_getCursorUSR(cursor)));
    clang_disposeString(kind_string);

    auto usr_string = clang_getCursorUSR(cursor);
    String my_usr_string = compiler->copy_string(to_string(clang_getCString(usr_string)));
    clang_disposeString(usr_string);

    auto location = clang_getCursorLocation(cursor);
    CXFile file;
    unsigned line;
    unsigned column;
    unsigned offset;
    clang_getFileLocation(location, &file, &line, &column, &offset);

    auto filename = copy_and_dispose(compiler, clang_getFileName(file));

    switch (cursor.kind) {
        case CXCursor_UnexposedAttr: {
            // @Hack this is usually just extern "C", but clang_Cursor_getMangling handles the linkage for us.
            return CXChildVisit_Recurse;
        }
        case CXCursor_FunctionDecl: {
            cursor = clang_getCursorDefinition(cursor);
            if (find_ast(usr_map, my_usr_string)) {
                break; // Skip, we've already filled this function
            }

            Ast_Function *function = IMPORT_NEW(Ast_Function);
            add_usr_mapping(usr_map, my_usr_string, function);

            function->is_c_function = true;
            function->is_c_varargs  = (clang_Cursor_isVariadic(cursor) != 0);

            CXString cxstring = clang_getCursorSpelling(cursor);
            defer { clang_disposeString(cxstring); };

            String name = to_string(clang_getCString(cxstring));

            Atom *name_atom = compiler->make_atom(name);
            function->identifier = make_identifier(compiler, name_atom);
            function->identifier->enclosing_scope = current_scope;

            int num_args = clang_Cursor_getNumArguments(cursor);
            for (int i = 0; i < num_args; ++i) {
                CXCursor param = clang_Cursor_getArgument(cursor, i);
                CXType param_type = clang_getCursorType(param);

                CXString cxstring = clang_getCursorSpelling(param);
                defer { clang_disposeString(cxstring); };

                Ast_Type_Info *type_info = get_jiyu_type(visitor_data, param_type);

                String name = to_string(clang_getCString(cxstring));
                Atom *name_atom = compiler->make_atom(name);
                Ast_Declaration *param_decl = IMPORT_NEW(Ast_Declaration);
                param_decl->identifier = make_identifier(compiler, name_atom);
                param_decl->type_info = type_info;

                function->arguments.add(param_decl);
            }

            {
                CXString cxstring = clang_Cursor_getMangling(cursor);
                defer { clang_disposeString(cxstring); };

                String name = to_string(clang_getCString(cxstring));
                function->linkage_name = compiler->copy_string(name);
            }

            // We should maybe make get_jiyu_type work for the function prototype type, but
            // doing this for now just to get things going.
            function->type_info = compiler->make_function_type(function);

            current_scope->statements.add(function);
            current_scope->declarations.add(function);
            break;
        }

        case CXCursor_TypedefDecl: {
            Ast_Type_Alias *alias = IMPORT_NEW(Ast_Type_Alias);
            add_usr_mapping(usr_map, my_usr_string, alias);
            alias->type_info = compiler->type_info_type;

            CXString cxstring = clang_getCursorSpelling(cursor);
            defer { clang_disposeString(cxstring); };

            String name = to_string(clang_getCString(cxstring));
            // printf("Alias name: %.*s\n", PRINT_ARG(name));

            Atom *name_atom = compiler->make_atom(name);
            alias->identifier = make_identifier(compiler, name_atom);
            alias->identifier->enclosing_scope = current_scope;

            CXType   underlying_type = clang_getTypedefDeclUnderlyingType(cursor);

            auto info = get_jiyu_type(visitor_data, underlying_type);
            assert(info);
            alias->type_value = compiler->make_type_alias(info);
            alias->type_value->alias_decl = alias;

            current_scope->statements.add(alias);
            current_scope->declarations.add(alias);
            break;
        }

        case CXCursor_EnumDecl: {
            // @TODO since we do not have enums yet, just import the type of the enum as
            // a typealias to the underlying C type and import enumerates as lets.
            Ast_Type_Alias *alias = IMPORT_NEW(Ast_Type_Alias);
            add_usr_mapping(usr_map, my_usr_string, alias);
            alias->type_info = compiler->type_info_type;

            CXString cxstring = clang_getCursorSpelling(cursor);
            defer { clang_disposeString(cxstring); };

            String name = to_string(clang_getCString(cxstring));
            // printf("Alias name: %.*s\n", PRINT_ARG(name));

            Atom *name_atom = compiler->make_atom(name);
            alias->identifier = make_identifier(compiler, name_atom);
            alias->identifier->enclosing_scope = current_scope;

            CXType underlying_type = clang_getEnumDeclIntegerType(cursor);

            auto info = get_jiyu_type(visitor_data, underlying_type);
            assert(info);
            alias->type_value = compiler->make_type_alias(info);
            alias->type_value->alias_decl = alias;

            current_scope->statements.add(alias);
            current_scope->declarations.add(alias);


            return CXChildVisit_Recurse; // @TODO This should just be a break and we can call cursor_visitor with the proper scope information to build the proper enum type.
        }

        case CXCursor_EnumConstantDecl: {
            Ast_Declaration *decl = IMPORT_NEW(Ast_Declaration);

            CXString cxstring = clang_getCursorSpelling(cursor);
            defer { clang_disposeString(cxstring); };

            String name = to_string(clang_getCString(cxstring));
            // printf("Field name: %.*s\n", PRINT_ARG(name));

            if (name != String()) {
                Atom *name_atom = compiler->make_atom(name);
                decl->identifier = make_identifier(compiler, name_atom);
                decl->identifier->enclosing_scope = current_scope;
            }

            CXType type = clang_getCursorType(cursor);
            Ast_Type_Info *info = get_jiyu_type(visitor_data, type);

            decl->is_let = true;
            decl->is_readonly_variable = false;
            // decl->is_struct_member = true; // This will be set via typechecking anyways..
            decl->type_info = info;

            current_scope->statements.add(decl);
            current_scope->declarations.add(decl);
            break;
        }

        case CXCursor_UnionDecl:
        case CXCursor_StructDecl: {
            cursor = clang_getCursorDefinition(cursor);
            if (find_ast(usr_map, my_usr_string)) {
                break; // Skip, we've already filled the type.
            }

            Ast_Struct *_struct = IMPORT_NEW(Ast_Struct);
            _struct->is_union = (cursor.kind == CXCursor_UnionDecl);
            _struct->type_value = make_struct_type(compiler, _struct);
            add_usr_mapping(usr_map, my_usr_string, _struct);

            {
                auto cursor_type = clang_getCursorType(cursor);
                auto type_value = _struct->type_value;
                type_value->size = clang_Type_getSizeOf(cursor_type);
                type_value->stride = type_value->size;
                type_value->alignment = clang_Type_getAlignOf(cursor_type);
            }

            _struct->member_scope.parent = current_scope;

            CXString cxstring = clang_getCursorSpelling(cursor);
            defer { clang_disposeString(cxstring); };

            String name = to_string(clang_getCString(cxstring));
            // printf("Struct name: %.*s\n", PRINT_ARG(name));

            if (name != String()) {
                Atom *name_atom = compiler->make_atom(name);
                _struct->identifier = make_identifier(compiler, name_atom);
                _struct->identifier->enclosing_scope = current_scope;
            }

            Visitor_Data data;
            data.compiler = compiler;
            data.target_scope = &_struct->member_scope;
            data.usr_map = usr_map;

            clang_visitChildren(cursor, cursor_visitor, &data);

            // @Incomplete add to type-map
            current_scope->statements.add(_struct);
            current_scope->declarations.add(_struct);
            break;
        }

        case CXCursor_FieldDecl: {
            Ast_Declaration *decl = IMPORT_NEW(Ast_Declaration);

            CXString cxstring = clang_getCursorSpelling(cursor);
            defer { clang_disposeString(cxstring); };

            String name = to_string(clang_getCString(cxstring));
            // printf("Field name: %.*s\n", PRINT_ARG(name));

            if (name != String()) {
                Atom *name_atom = compiler->make_atom(name);
                decl->identifier = make_identifier(compiler, name_atom);
                decl->identifier->enclosing_scope = current_scope;
            }

            CXType type = clang_getCursorType(cursor);
            Ast_Type_Info *info = get_jiyu_type(visitor_data, type);

            decl->is_let = false;
            decl->is_readonly_variable = false;
            // decl->is_struct_member = true; // This will be set via typechecking anyways..
            decl->type_info = info;

            current_scope->statements.add(decl);
            current_scope->declarations.add(decl);
            break;
        }

        default: {
            CXString cxstring = clang_getCursorSpelling(cursor);
            defer { clang_disposeString(cxstring); };

            String name = to_string(clang_getCString(cxstring));
            printf("Unahndled CXCursor in cursor_visitor. kind: %s, name: %.*s. This is a message for maintainers.\n", clang_getCString(kind_string), PRINT_ARG(name));
            break;
        }
    }

    return CXChildVisit_Continue;
}


bool perform_clang_import(Compiler *compiler, char *c_filepath, Ast_Scope *target_scope) {
    CXIndex index = clang_createIndex(/*excludeDeclarationsFromPCH=*/0, /*displayDiagnostics=*/0);

    // @Incomplete
    Array<char *> clang_command_line_args;
    for (auto lib_search_path: compiler->library_search_paths) {
        clang_command_line_args.add("-I");
        clang_command_line_args.add(to_c_string(lib_search_path)); // @Leak
    }

    CXTranslationUnit translation_unit;
    CXErrorCode error = clang_parseTranslationUnit2(index,
                            c_filepath,
                            clang_command_line_args.data,
                            clang_command_line_args.count,
                            /*unsaved_files=*/nullptr,
                            /*num_unsaved_files=*/0,
                            CXTranslationUnit_None,
                            &translation_unit);

    defer {
        clang_disposeTranslationUnit(translation_unit);
        clang_disposeIndex(index);
    };

    if (error != 0) return false;

    bool has_errors = false;
    for (unsigned i = 0; i < clang_getNumDiagnostics(translation_unit); ++i) {
        CXDiagnostic diag = clang_getDiagnostic(translation_unit, i);
        CXString diag_string = clang_formatDiagnostic(diag, CXDiagnostic_DisplaySourceLocation | CXDiagnostic_DisplayColumn);
        
        // @Incomplete what we really want is to use Jiyu's error reporting system.
        printf("%s\n", clang_getCString(diag_string));
        clang_disposeString(diag_string);

        auto severity = clang_getDiagnosticSeverity(diag);
        if ((severity == CXDiagnostic_Error) || (severity == CXDiagnostic_Fatal)) {
            compiler->errors_reported += 1;
        }
    }

    if (compiler->errors_reported) return false;

    Array<USR_Pair>    usr_map;
    Visitor_Data data;
    data.compiler     = compiler;
    data.target_scope = target_scope;
    data.usr_map = &usr_map;

    // Add __builtin_va_list definition (officially *void under GCC/Clang, idk if this is true under Windows for Clang @TODO)
    {
        // @Cutnpaste from the cursor_visitor
        Ast_Type_Alias *alias = IMPORT_NEW(Ast_Type_Alias);
        add_usr_mapping(&usr_map, to_string("c:@T@__builtin_va_list"), alias);
        alias->type_info = compiler->type_info_type;

        auto info = compiler->type_ptr_void;
        alias->type_value = compiler->make_type_alias(info);
        alias->type_value->alias_decl = alias;

        target_scope->statements.add(alias);
        target_scope->declarations.add(alias);
    }

    clang_visitChildren(clang_getTranslationUnitCursor(translation_unit), cursor_visitor, &data);

    return true;
}
