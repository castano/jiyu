
#ifndef COMPILER_H
#define COMPILER_H

#include "general.h"
#include "ast.h"
#include "compiler_api.h"
#include "lexer.h"

#include <stdarg.h>

struct Token;
struct Span;
struct LLVM_Generator;
struct LLVM_Jitter;
struct Sema;
struct Copier;

const String OPERATOR_BRACKET_NAME        = to_string("__operator[]");
const String OPERATOR_BRACKET_EQUALS_NAME = to_string("__operator[]=");
const String BUILTIN_DEBUGTRAP_NAME      = to_string("__builtin_debugtrap");

struct Atom {
    String name;
    u32 hash;
};

struct Atom_Table {
    Array<Atom *> data;


    Atom *find_atom(String name) {
        u32 hash = hash_key(name);
        for (array_count_type i = 0; i < data.count; ++i) {
            auto it = data[i];
            if (it->hash == hash) {
                if (it->name == name) return it;
            }
        }

        return nullptr;
    }

    u32 hash_key(String str) {
        u32 hash = 5381;
        s16 c = 0;;

        for (string_length_type i = 0; i < str.length; ++i) {
            // double cast to ensure sign extension
            c =  (s16) (s8) str[i];
            hash = ((hash << 5) + hash) + (u32) c;
        }

        return hash;
    }
};

// @Volatile must match Compiler.jyu stuff
struct Compiler {
    bool is_metaprogram = false;
    s64 errors_reported = 0;

    s64 instance_number = -1;
    Array<Ast_Library *> libraries;
    Array<String> library_search_paths;
    Array<String> module_search_paths;
    Array<String> user_supplied_objs;
    Array<Ast_Directive_Import *> loaded_imports;
    Build_Options build_options;

    s64 pointer_size = -1; // @TargetInfo

    Sema *sema;
    Copier *copier;
    LLVM_Generator *llvm_gen;
    LLVM_Jitter    *jitter;

    Atom_Table *atom_table;

    Ast_Scope *preload_scope;
    Ast_Scope *global_scope;

    Ast_Type_Info *type_void;
    Ast_Type_Info *type_bool;
    Ast_Type_Info *type_int8;
    Ast_Type_Info *type_int16;
    Ast_Type_Info *type_int32;
    Ast_Type_Info *type_int64;

    Ast_Type_Info *type_uint8;
    Ast_Type_Info *type_uint16;
    Ast_Type_Info *type_uint32;
    Ast_Type_Info *type_uint64;

    Ast_Type_Info *type_float32;
    Ast_Type_Info *type_float64;
    Ast_Type_Info *type_float128;

    Ast_Type_Info *type_string;
    Ast_Type_Info *type_string_data;
    Ast_Type_Info *type_string_length;

    Ast_Type_Info *type_array_count;
    Ast_Type_Info *type_array_count_unsigned;

    Ast_Type_Info *type_info_type;

    Ast_Type_Info *type_ptr_void;

    Atom *atom_data;
    Atom *atom_length;
    Atom *atom_count;
    Atom *atom_allocated;
    Atom *atom_it;
    Atom *atom_it_index;
    Atom *atom_main;
    Atom *atom___strings_match;
    Atom *atom_os;
    Atom *atom_MacOSX;
    Atom *atom_Windows;
    Atom *atom_Linux;

    Atom *atom_builtin_debugtrap;

    Array<Ast_Function    *> function_emission_queue;
    Array<Ast_Declaration *> global_decl_emission_queue;
    Array<Ast_Directive   *> directive_queue;

    Array<Ast_Type_Info   *> type_table;

    Pool memory_pool;

    Compiler() {
        atom_table = new Atom_Table();   // @Leak
        preload_scope = new Ast_Scope(); // @Leak
        global_scope  = new Ast_Scope(); // @Leak
        global_scope->parent = preload_scope;
    }

    char *get_temp_c_string(String s);

    void init();

    void *get_memory(array_count_type amount);
    String copy_string(String s);

    // All these functions add to the type table before returning. Their results should not be modified!
    Ast_Type_Info *make_pointer_type(Ast_Type_Info *pointee);
    Ast_Type_Info *make_type_alias_type(Ast_Type_Info *aliasee);
    Ast_Type_Info *make_array_type(Ast_Type_Info *element, array_count_type count, bool is_dynamic);
    Ast_Type_Info *make_function_type(Ast_Function *function);
    Ast_Type_Info *make_enum_type(Ast_Enum *_enum);
    void add_to_type_table(Ast_Type_Info *info);

    // _name_ is internally copied.
    Atom *make_atom(String name);
    Atom *make_operator_atom(Token::Type operator_type);

    void queue_directive(Ast_Directive *directive);
    void resolve_directives();

    void report_diagnostic_valist(String filename, String source, Span error_location, char *level_name, char *fmt, va_list args);
    void report_error(Token *tok, char *fmt, ...);
    void report_error(Ast *ast, char *fmt, ...);

    void report_warning(Token *tok, char *fmt, ...);
    void report_warning(Ast *ast, char *fmt, ...);

    bool is_toplevel_scope(Ast_Scope *scope);

    Tuple<bool, String> find_file_in_library_search_paths(String filename);
};

// Structs must be added to the type table manually
Ast_Type_Info *make_struct_type(Compiler *compiler, Ast_Struct *_struct);

Ast_Type_Info *get_final_type(Ast_Type_Info *info);
bool types_match(Ast_Type_Info *left, Ast_Type_Info *right);

inline
bool is_valid_overloadable_operator(Token::Type op) {
    return op == Token::MINUS || op == Token::PLUS || op == Token::STAR || op == Token::SLASH || op == Token::EQUALS;
}

inline
bool is_int_type(Ast_Type_Info *info) {
    info = get_final_type(info);
    return info->type == Ast_Type_Info::INTEGER;
}

inline
bool is_int_or_enum_type(Ast_Type_Info *info) {
    info = get_final_type(info);
    return info->type == Ast_Type_Info::INTEGER || info->type == Ast_Type_Info::ENUM;
}

inline
bool is_float_type(Ast_Type_Info *info) {
    info = get_final_type(info);
    return info->type == Ast_Type_Info::FLOAT;
}

inline
bool is_pointer_type(Ast_Type_Info *info) {
    info = get_final_type(info);
    return info->type == Ast_Type_Info::POINTER;
}

inline
bool is_struct_type(Ast_Type_Info *info) {
    info = get_final_type(info);
    return info->type == Ast_Type_Info::STRUCT;
}

inline
bool is_enum_type(Ast_Type_Info *info) {
    info = get_final_type(info);
    return info->type == Ast_Type_Info::ENUM;
}

inline
bool is_array_type(Ast_Type_Info *info) {
    info = get_final_type(info);
    return info->type == Ast_Type_Info::ARRAY;
}

inline
bool is_aggregate_type(Ast_Type_Info *info) {
    info = get_final_type(info);
    return info->type == Ast_Type_Info::STRUCT || info->type == Ast_Type_Info::STRING;
}

inline
bool is_function_type(Ast_Type_Info *info) {
    info = get_final_type(info);
    return info->type == Ast_Type_Info::FUNCTION;
}

inline
Ast_Type_Info *get_type_info(Ast_Expression *expr) {
    while (expr->substitution) expr = expr->substitution;

    return expr->type_info;
}


inline
bool is_valid_primitive_cast(Ast_Type_Info *target, Ast_Type_Info *source) {
    target = get_final_type(target);
    source = get_final_type(source);

    if (target->type == Ast_Type_Info::POINTER) {
        return (source->type == Ast_Type_Info::INTEGER || source->type == Ast_Type_Info::POINTER);
    }

    if (target->type == Ast_Type_Info::FUNCTION) {
        return source->type == Ast_Type_Info::POINTER || source->type == Ast_Type_Info::FUNCTION;
    }

    if (target->type == Ast_Type_Info::INTEGER) {
        return (source->type == Ast_Type_Info::INTEGER || source->type == Ast_Type_Info::POINTER || source->type == Ast_Type_Info::FLOAT || source->type == Ast_Type_Info::ENUM);
    }

    if (target->type == Ast_Type_Info::FLOAT) {
        return (source->type == Ast_Type_Info::FLOAT || source->type == Ast_Type_Info::INTEGER);
    }

    if (target->type == Ast_Type_Info::ENUM) {
        return (source->type == Ast_Type_Info::INTEGER) || (source->type == Ast_Type_Info::ENUM);
    }

    return false;
}

inline
Ast_Literal *resolves_to_literal_value(Ast_Expression *expr) {
    while (expr->substitution) expr = expr->substitution;

    if (expr->type == AST_LITERAL) return static_cast<Ast_Literal *>(expr);

    return nullptr;
}

inline
void copy_location_info(Ast *left, Ast *right) {
    left->text_span = right->text_span;
    left->filename  = right->filename;
}

inline
bool is_a_type_declaration(Ast *expression) {
    return expression->type == AST_STRUCT || expression->type == AST_TYPE_ALIAS || expression->type == AST_ENUM;
}

inline
Ast_Type_Info *
get_type_declaration_resolved_type(Ast *expression) {
    assert(is_a_type_declaration(expression));

    if (expression->type == AST_STRUCT) {
        auto _struct = static_cast<Ast_Struct *>(expression);
        return _struct->type_value;
    } else if (expression->type == AST_TYPE_ALIAS) {
        auto alias = static_cast<Ast_Type_Alias *>(expression);
        return alias->type_value;
    } else if (expression->type == AST_ENUM) {
        auto _enum = static_cast<Ast_Enum *>(expression);
        return _enum->type_value;
    }

    return nullptr;
}


Ast_Expression *cast_int_to_int(Compiler *compiler, Ast_Expression *expr, Ast_Type_Info *target);

Ast_Expression *cast_float_to_float(Compiler *compiler, Ast_Expression *expr, Ast_Type_Info *target);

Ast_Expression *cast_int_to_float(Compiler *compiler, Ast_Expression *expr, Ast_Type_Info *target);

Ast_Expression *cast_ptr_to_ptr(Compiler *compiler, Ast_Expression *expr, Ast_Type_Info *target);

Ast_Literal *make_string_literal(Compiler *compiler, String value, Ast *source_loc = nullptr);

Ast_Literal *make_integer_literal(Compiler *compiler, s64 value, Ast_Type_Info *type_info, Ast *source_loc = nullptr);

Ast_Literal *make_float_literal(Compiler *compiler, double value, Ast_Type_Info *type_info, Ast *source_loc = nullptr);

Ast_Literal *make_bool_literal(Compiler *compiler, bool value, Ast *source_loc = nullptr);

Ast_Literal *make_null_literal(Compiler *compiler, Ast_Type_Info *pointer_type, Ast *source_loc = nullptr);

Ast_Identifier *make_identifier(Compiler *compiler, Atom *name);

// @Note MUST typecheck the return value of this!!!
Ast_Array_Dereference *make_array_index(Compiler *compiler, Ast_Expression *array, Ast_Expression *index);

// @Note MUST typecheck the return value of this!!!
Ast_Dereference *make_dereference(Compiler *compiler, Ast_Expression *aggregate_expression, Atom *field);

// @Note MUST typecheck the return value of this!!!
Ast_Unary_Expression *make_unary(Compiler *compiler, Token::Type op, Ast_Expression *subexpr);

Ast_Binary_Expression *make_binary(Compiler *compiler, Token::Type op, Ast_Expression *left, Ast_Expression *right, Ast *location);

Ast_Type_Alias *make_type_alias(Compiler *compiler, Ast_Identifier *ident, Ast_Type_Info *type_value);
Ast_Function *make_function(Compiler *compiler, Ast_Identifier *ident);

#endif
