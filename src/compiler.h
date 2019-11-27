
#ifndef COMPILER_H
#define COMPILER_H

#include "general.h"
#include "ast.h"

#include <stdarg.h>

struct Token;
struct Span;
struct LLVM_Generator;
struct Sema;
struct Copier;

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
struct Build_Options {
    String executable_name;
    String target_triple;
    bool only_want_obj_file = false;
};

// @Volatile must match Compiler.jyu stuff
struct Compiler {
    bool is_metaprogram = false;
    s64 errors_reported = 0;

    s64 instance_number = -1;
    Array<Ast_Library *> libraries;
    Array<String> library_search_paths;
    Array<String> module_search_paths;
    Array<Ast_Directive_Import *> loaded_imports;
    Build_Options build_options;

    s64 pointer_size = -1; // @TargetInfo

    s32 metaprogram_argc = 0;
    char **metaprogram_argv = nullptr;

    Sema *sema;
    Copier *copier;
    LLVM_Generator *llvm_gen;
    
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
    
    Ast_Type_Info *type_string;
    Ast_Type_Info *type_string_data;
    Ast_Type_Info *type_string_length;
    
    Ast_Type_Info *type_array_count;
    
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

    // All these functions add to the type table before returning. Their results should not be modified!
    Ast_Type_Info *make_pointer_type(Ast_Type_Info *pointee);
    Ast_Type_Info *make_array_type(Ast_Type_Info *element, array_count_type count, bool is_dynamic);
    Ast_Type_Info *make_function_type(Ast_Function *function);
    Ast_Type_Info *make_enum_type(Ast_Enum *_enum);
    void add_to_type_table(Ast_Type_Info *info);
    
    Atom *make_atom(String name);
    
    void queue_directive(Ast_Directive *directive);
    void resolve_directives();
    
    void report_error_valist(String filename, String source, Span error_location, char *fmt, va_list args);
    void report_error(Token *tok, char *fmt, ...);
    void report_error(Ast *ast, char *fmt, ...);

    bool is_toplevel_scope(Ast_Scope *scope);
};

// Structs must be added to the type table manually
Ast_Type_Info *make_struct_type(Ast_Struct *_struct);

bool types_match(Ast_Type_Info *left, Ast_Type_Info *right);

inline
bool is_int_type(Ast_Type_Info *info) {
    if (info->type == Ast_Type_Info::ALIAS) return is_int_type(info->alias_of);
    return info->type == Ast_Type_Info::INTEGER;
}

inline
bool is_float_type(Ast_Type_Info *info) {
    if (info->type == Ast_Type_Info::ALIAS) return is_float_type(info->alias_of);
    return info->type == Ast_Type_Info::FLOAT;
}

inline
bool is_pointer_type(Ast_Type_Info *info) {
    if (info->type == Ast_Type_Info::ALIAS) return is_pointer_type(info->alias_of);

    return info->type == Ast_Type_Info::POINTER;
}

inline
bool is_struct_type(Ast_Type_Info *info) {
    if (info->type == Ast_Type_Info::ALIAS) return is_struct_type(info->alias_of);

    return info->type == Ast_Type_Info::STRUCT;
}

inline
bool is_aggregate_type(Ast_Type_Info *info) {
    if (info->type == Ast_Type_Info::ALIAS) return is_aggregate_type(info->alias_of);
    // @TODO arrays
    return info->type == Ast_Type_Info::STRUCT || info->type == Ast_Type_Info::STRING;
}

inline
Ast_Type_Info *get_type_info(Ast_Expression *expr) {
    while (expr->substitution) expr = expr->substitution;
    
    return expr->type_info;
}

inline
bool is_valid_primitive_cast(Ast_Type_Info *target, Ast_Type_Info *source) {
    if (target->type == Ast_Type_Info::POINTER) {
        return (source->type == Ast_Type_Info::INTEGER || source->type == Ast_Type_Info::POINTER);
    }
    
    if (target->type == Ast_Type_Info::FUNCTION) {
        return source->type == Ast_Type_Info::POINTER || source->type == Ast_Type_Info::FUNCTION;
    }
    
    if (target->type == Ast_Type_Info::INTEGER) {
        return (source->type == Ast_Type_Info::INTEGER || source->type == Ast_Type_Info::POINTER || source->type == Ast_Type_Info::FLOAT);
    }
    
    if (target->type == Ast_Type_Info::FLOAT) {
        return (source->type == Ast_Type_Info::FLOAT || source->type == Ast_Type_Info::INTEGER);
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
Ast_Type_Info *get_final_type(Ast_Type_Info *info) {
    if (!info) return nullptr;

    if (info->type == Ast_Type_Info::ALIAS) {
        return get_final_type(info->alias_of);
    }

    return info;
}

inline
bool is_a_type_declaration(Ast_Expression *expression) {
    return expression->type == AST_STRUCT || expression->type == AST_TYPE_ALIAS;
}

inline
Ast_Type_Info *
get_type_declaration_resolved_type(Ast_Expression *expression) {
    assert(is_a_type_declaration(expression));

    if (expression->type == AST_STRUCT) {
        auto _struct = static_cast<Ast_Struct *>(expression);
        return _struct->type_value;
    } else if (expression->type == AST_TYPE_ALIAS) {
        auto alias = static_cast<Ast_Type_Alias *>(expression);
        return alias->type_value;
    }

    return nullptr;
}

#endif
