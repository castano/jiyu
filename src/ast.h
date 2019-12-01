
#ifndef AST_H
#define AST_H

#include "general.h"
#include "lexer.h"

struct Atom;
struct Ast_Declaration;
struct Ast_Function;
struct Ast_Type_Info;
struct Ast_Identifier;
struct Ast_Scope;
struct Ast_Struct;
struct Ast_Type_Alias;
struct Ast_Enum;

enum Ast_Type {
    AST_UNINITIALIZED,
    AST_BINARY_EXPRESSION,
    AST_UNARY_EXPRESSION,
    AST_IDENTIFIER,
    AST_DECLARATION,
    AST_SCOPE,
    AST_FUNCTION,
    AST_LITERAL,
    AST_FUNCTION_CALL,
    AST_DEREFERENCE,
    AST_CAST,
    AST_IF,
    AST_WHILE,
    AST_RETURN,
    AST_TYPE_INSTANTIATION,
    AST_TYPE_ALIAS,
    AST_ARRAY_DEREFERENCE,
    AST_SIZEOF,
    AST_FOR,
    AST_STRUCT,
    AST_ENUM,
    AST_DIRECTIVE_LOAD,
    AST_DIRECTIVE_IMPORT,
    AST_DIRECTIVE_STATIC_IF,
    AST_SCOPE_EXPANSION,
    AST_OS,
    AST_LIBRARY,
    AST_CONTROL_FLOW,
};

struct Ast {
    TextSpan text_span;
    String filename;
    
    Ast_Type type;
};

struct Ast_Type_Info {
    enum Type {
        UNINITIALIZED = 0,
        
        // primitves
        VOID,
        BOOL,
        INTEGER,
        FLOAT,
        STRING,
        
        // non-primitves
        POINTER,
        ARRAY,
        ALIAS,
        FUNCTION,
        TYPE, // meta type assigned to typealias and struct
        
        // User defined (mostly)
        STRUCT,
        ENUM,
    };
    
    Type type = UNINITIALIZED;
    
    
    bool is_signed = false; // for INTEGER
    
    Ast_Type_Info *pointer_to    = nullptr;
    Ast_Type_Info *array_element = nullptr;
    
    Ast_Type_Alias *alias_decl    = nullptr;
    Ast_Type_Info  *alias_of      = nullptr;

    array_count_type array_element_count = -1;
    bool is_dynamic = false; // for array
    
    Ast_Struct *struct_decl = nullptr;
    // @Cleanup hmm... is this really necessary? Should we just have all type code just check struct_decl? or is that overstepping what the type information is for..?
    struct Struct_Member {
        Atom *name               = nullptr;
        s64 element_index        = -1;
        s64 offset_in_struct     = -1;
        Ast_Type_Info *type_info = nullptr;
        bool is_let = false;
    };
    Array<Struct_Member> struct_members; // for STRUCT

    // FUNCTION
    Array<Ast_Type_Info *> arguments;
    Ast_Type_Info *return_type = nullptr;
    bool is_c_function = false;
    bool is_c_varargs  = false;

    Ast_Enum *enum_decl = nullptr;
    Ast_Type_Info *enum_base_type = nullptr;

    // @@ Generate this info!
    struct Enum_Member {
        Atom *name;
        u64 value;
    };
    Array<Enum_Member> enum_member;
    
    s64 stride = -1;
    s64 alignment = -1;
    s64 size = -1;

    array_count_type type_table_index = -1; // Mostly used for LLVM for now. @Cleanup @TODO actually make a type table system.
    array_count_type debug_type_table_index = -1; // @Cleanup this should not be necessary once we have a real type table.
};

struct Ast_Expression : Ast {
    Ast_Type_Info *type_info = nullptr;
    
    Ast_Expression *substitution = nullptr;
};

struct Ast_Scope : Ast_Expression {
    Ast_Scope() { type = AST_SCOPE; }
    Ast_Scope *parent = nullptr;
    Array<Ast_Expression *> statements;
    Array<Ast_Expression *> declarations;         // @NoCopy filled by copy_scope depending on the result of is_declaration().
    Array<Ast_Expression *> private_declarations; // The members of this should be lite-copied only. This is only used for scope-expansions inserted by Compiler::resolve_directives().
    
    bool is_template_argument_block = false;
    bool rejected_by_static_if = false;
    
    Ast_Struct *owning_struct = nullptr; // @NoCopy set by the code that copies Ast_Struct


    // Only set for the top-level scope of a function body. scope->owning_function == scope->owning_function->scope.
    Ast_Function   *owning_function = nullptr;  // @NoCopy this is set on the root scope by Copier::copy_function
    Ast_Expression *owning_statement = nullptr; // @NoCopy this is set by respective copying code for owner-nodes (Ast_For ...).
    Ast_Enum *owning_enum = nullptr;
};

// Used to specify a scope that was inserted due to the compiler resolving a static_if.
struct Ast_Scope_Expansion : Ast_Expression {
    Ast_Scope_Expansion() { type = AST_SCOPE_EXPANSION; }
    
    // Only copy this if expanded_via_import_directive is nullptr since
    // it came from a static_if, otherwise this will point to the entire scope of a #import.
    Ast_Scope *scope = nullptr;
    Ast_Expression *expanded_via_import_directive = nullptr; // @NoCopy
};

struct Ast_Type_Instantiation : Ast_Expression {
    Ast_Type_Instantiation() { type = AST_TYPE_INSTANTIATION; }
    
    // @Cleanup maybe, builtin_primitive sort of overlaps with type_value since it is known at the time of parsing.
    Ast_Type_Info *builtin_primitive = nullptr;
    Ast_Type_Instantiation *pointer_to = nullptr;
    Ast_Identifier *typename_identifier = nullptr;
    
    Ast_Type_Instantiation *array_element_type = nullptr;
    Ast_Expression *array_size_expression = nullptr;
    bool array_is_dynamic = false;
    
    Ast_Function *function_header = nullptr;
    
    Ast_Type_Info *type_value = nullptr; // @NoCopy
};

struct Ast_Type_Alias : Ast_Expression {
    Ast_Type_Alias() { type = AST_TYPE_ALIAS; }
    
    Ast_Identifier *identifier = nullptr;
    Ast_Type_Instantiation *internal_type_inst = nullptr;
    
    Ast_Type_Info *type_value = nullptr; // use this instead of internal_type_inst->type_value in case this typealias was generated by the compiler by template-instantiation.
};

struct Ast_Struct : Ast_Expression {
    Ast_Struct() { type = AST_STRUCT; }
    
    Ast_Identifier *identifier = nullptr;
    Ast_Scope member_scope;
    
    Ast_Type_Info *type_value = nullptr; // @NoCopy
};

struct Ast_Enum : Ast_Expression {
    Ast_Enum() { type = AST_ENUM; }
    
    Ast_Identifier *identifier = nullptr;
    Ast_Type_Instantiation *base_type = nullptr;
    Ast_Scope member_scope;
    
    Ast_Type_Info *type_value = nullptr;
};

struct Ast_Unary_Expression : Ast_Expression {
    Ast_Unary_Expression() { type = AST_UNARY_EXPRESSION; }
    
    Token::Type operator_type;
    Ast_Expression *expression = nullptr;
};

struct Ast_Binary_Expression : Ast_Expression {
    Ast_Binary_Expression() { type = AST_BINARY_EXPRESSION; }
    
    Token::Type operator_type;
    Ast_Expression *left  = nullptr;
    Ast_Expression *right = nullptr;
};

struct Ast_Identifier : Ast_Expression {
    Ast_Identifier() { type = AST_IDENTIFIER; }
    Atom *name = nullptr;
    
    Ast_Scope *enclosing_scope = nullptr; // @NoCopy should be set to the current scope being polymorphed.
    
    Ast_Expression *resolved_declaration = nullptr; // @NoCopy
    Array<Ast_Function *> overload_set;             // @NoCopy
};

struct Ast_Dereference : Ast_Expression {
    Ast_Dereference() { type = AST_DEREFERENCE; }
    
    Ast_Expression *left = nullptr;
    Ast_Identifier *field_selector = nullptr;

    // Same as left, except retains the original value of left if left gets transformed to something
    // else. @Cleanup we should be able to avoid this if we can do left->substitute.
    Ast_Expression *original_left = nullptr; // @NoCopy

    bool is_type_dereference = false;  // @NoCopy
    s64 element_path_index = -1;       // @NoCopy 0-based element into the list of declarations or fields within the outer type
    s64 byte_offset = -1;              // @NoCopy byte-offset from the start of the the memory occupied by the outer type
};

struct Ast_Array_Dereference : Ast_Expression {
    Ast_Array_Dereference() { type = AST_ARRAY_DEREFERENCE; }
    
    Ast_Expression *array_or_pointer_expression = nullptr;
    Ast_Expression *index_expression = nullptr;
};

struct Ast_Function_Call : Ast_Expression {
    Ast_Function_Call() { type = AST_FUNCTION_CALL; }
    
    Ast_Expression *function_or_function_ptr = nullptr;
    Array<Ast_Expression *> argument_list;

    bool implicit_argument_inserted = false; // @NoCopy
};

struct Ast_If : Ast_Expression {
    Ast_If() { type = AST_IF; }
    
    Ast_Expression *condition = nullptr;
    
    // These should probably be Ast_Scope by default..
    Ast_Expression *then_statement = nullptr;
    Ast_Expression *else_statement = nullptr;
};

struct Ast_While : Ast_Expression {
    Ast_While() { type = AST_WHILE; }
    
    Ast_Expression *condition = nullptr;
    Ast_Expression *statement = nullptr;
};

struct Ast_Return : Ast_Expression {
    Ast_Return() { type = AST_RETURN; }
    
    Ast_Expression *expression = nullptr;
    
    // @TODO this could be found via a scope lookup like Ast_Control_Flow.
    Ast_Function *owning_function = nullptr; // @NoCopy should be set to the current function being polymorphed.
};

struct Ast_Literal : Ast_Expression {
    Ast_Literal() { type = AST_LITERAL; }
    
    enum Type {
        INTEGER,
        STRING,
        FLOAT,
        BOOL,
        NULLPTR,
    };
    
    Type literal_type;
    
    bool bool_value;
    s64 integer_value;
    double float_value;
    String string_value;
};

struct Ast_Declaration : Ast_Expression {
    Ast_Declaration() { type = AST_DECLARATION; }
    Ast_Identifier *identifier = nullptr;
    Ast_Expression *initializer_expression = nullptr;
    
    Ast_Type_Instantiation *type_inst = nullptr;
    
    bool is_let = false;
    bool is_readonly_variable = false;
    bool is_struct_member = false;
    bool is_enum_member = false;
};

struct Ast_Function : Ast_Expression {
    Ast_Function() { type = AST_FUNCTION; }
    
    Ast_Identifier *identifier;
    
    Array<Ast_Declaration *> arguments;
    Ast_Declaration *return_decl = nullptr; // @FixMe this should be a Ast_Type_Instantiation not a declaration.
    
    Ast_Scope *polymorphic_type_alias_scope = nullptr;
    Ast_Scope arguments_scope;
    Ast_Scope *scope = nullptr; // Function body. @Cleanup Maybe this should be renamed "body".
    
    Array<Ast_Function *> polymorphed_overloads; // @NoCopy
    
    bool is_marked_metaprogram = false;
    bool is_c_function = false;
    bool is_c_varargs = false;
    bool is_template_function = false;
    
    String linkage_name;       // @NoCopy
    bool body_checked = false; // @NoCopy
};

struct Ast_Cast : Ast_Expression {
    Ast_Cast() { type = AST_CAST; }
    Ast_Type_Instantiation *target_type_inst = nullptr;
    Ast_Expression *expression = nullptr;
};

struct Ast_Sizeof : Ast_Expression {
    Ast_Sizeof() { type = AST_SIZEOF; }
    Ast_Type_Instantiation *target_type_inst = nullptr;
};

struct Ast_Os : Ast_Expression {
    Ast_Os() { type = AST_OS; }
    
    Ast_Expression *expression = nullptr;
};

struct Ast_For : Ast_Expression {
    Ast_For() { type = AST_FOR; }
    
    bool is_element_pointer_iteration = false; // for * my_array { /*..*/ } // "it" is a writable pointer instead of a readonly copy.
    bool is_exclusive_end = false;             // for ..< ranges

    Ast_Declaration *iterator_decl       = nullptr;
    Ast_Declaration *iterator_index_decl = nullptr;
    
    Ast_Expression *initial_iterator_expression = nullptr;
    Ast_Expression *upper_range_expression      = nullptr;
    
    Ast_Scope iterator_declaration_scope; // @NoCopy filled by Sema
    Ast_Scope body;
};

struct Ast_Control_Flow : Ast_Expression {
    Ast_Control_Flow() { type = AST_CONTROL_FLOW; }

    Token::Type control_type;

    Ast_Scope *current_scope = nullptr; // @NoCopy Scope that the statement was made in, not necessarily the scope that target_statement may own...

    // Filled in by sema
    Ast_Expression *target_statement = nullptr; // @NoCopy
};

// :NoCopyDirectives:
// @NoCopy for directives because they are fully resolved before any semantic analysis can occur.
// And their resolution is static for the result of compilation.
struct Ast_Directive : Ast_Expression {
    
    Ast_Scope *scope_i_belong_to = nullptr;
};

// :NoCopyDirectives:
struct Ast_Directive_Load : Ast_Directive {
    Ast_Directive_Load() { type = AST_DIRECTIVE_LOAD; }
    
    Ast_Scope *target_scope;
    String     target_filename;
};

// :NoCopyDirectives:
struct Ast_Directive_Import : Ast_Directive {
    Ast_Directive_Import() { type = AST_DIRECTIVE_IMPORT; }

    Ast_Scope *target_scope;
    Ast_Scope *imported_scope = nullptr;
    String     target_filename;
};

// :NoCopyDirectives:
struct Ast_Directive_Static_If : Ast_Directive {
    Ast_Directive_Static_If() { type = AST_DIRECTIVE_STATIC_IF; }
    
    Ast_Expression *condition = nullptr;
    
    Ast_Scope *then_scope = nullptr;
    Ast_Scope *else_scope = nullptr;
};


struct Ast_Library : Ast_Expression {
    Ast_Library() { type = AST_LIBRARY; }

    bool is_framework = false;
    String libname;
};

inline
bool is_declaration(Ast_Type type) {
    return type == AST_DECLARATION
        || type == AST_FUNCTION
        || type == AST_TYPE_ALIAS
        || type == AST_STRUCT
        || type == AST_ENUM;
}

inline
Ast_Identifier * declaration_identifier(Ast_Expression * decl) {
    if (decl->type == AST_DECLARATION) return static_cast<Ast_Declaration*>(decl)->identifier;
    if (decl->type == AST_FUNCTION) return static_cast<Ast_Function*>(decl)->identifier;
    if (decl->type == AST_TYPE_ALIAS) return static_cast<Ast_Type_Alias*>(decl)->identifier;
    if (decl->type == AST_STRUCT) return static_cast<Ast_Struct*>(decl)->identifier;
    return NULL;
}

#endif
