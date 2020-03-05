
#ifndef SEMA_H
#define SEMA_H

#include "general.h"

struct Compiler;
struct Ast;
struct Ast_Scope;
struct Ast_Expression;
struct Ast_Function;
struct Ast_Type_Info;
struct Atom;
struct Ast_Type_Instantiation;
struct Ast_Function_Call;
struct Ast_Literal;
struct Ast_Struct;

struct Sema {
    Compiler *compiler;

    Sema(Compiler *compiler) {
        this->compiler = compiler;
    }

    Array<Ast_Expression *> expression_stack;

    Ast_Literal *folds_to_literal(Ast_Expression *expression);

    Ast_Function *get_polymorph_for_function_call(Ast_Function *template_function, Ast_Function_Call *call, bool do_errors);
    Ast_Struct   *get_polymorph_for_struct(Ast_Struct *_struct, Array<Ast_Type_Instantiation *> &type_arguments, Ast *site);

    Tuple<bool, u64> function_call_is_viable(Ast_Function_Call *call, Ast_Type_Info *function_type, Ast_Function *source, bool do_errors);
    Ast_Function *get_best_overload_from_set(Ast_Function_Call *call, Array<Ast_Function *> &overload_set);
    void collect_function_overloads_for_atom_in_scope(Atom *atom, Ast_Scope *start, Array<Ast_Function *> *overload_set, bool check_private_declarations = true);
    void collect_function_overloads_for_atom(Atom *atom, Ast_Scope *start, Array<Ast_Function *> *overload_set, bool check_private_declarations = true);
    Ast_Expression *find_declaration_for_atom(Atom *atom, Ast_Scope *start, bool check_private_declarations = true);
    Ast_Expression *find_declaration_for_atom_in_scope(Ast_Scope *scope, Atom *atom, bool check_private_declarations = true);

    Ast_Type_Info *resolve_type_inst(Ast_Type_Instantiation *type_inst);

    void typecheck_scope(Ast_Scope *scope);
    Tuple<u64, Ast_Expression *> typecheck_and_implicit_cast_single_expression(Ast_Expression *expression, Ast_Type_Info *target_type_info, u32 allow_flags);
    Tuple<u64, u64> typecheck_and_implicit_cast_expression_pair(Ast_Expression *left, Ast_Expression *right, Ast_Expression **result_left, Ast_Expression **result_right, bool allow_coerce_to_ptr_void);
    void typecheck_expression(Ast_Expression *expression, Ast_Type_Info *want_numeric_type = nullptr, bool overload_set_allowed = false, bool do_function_body = false, bool only_want_struct_type = true);

    void typecheck_function_header(Ast_Function *function, bool is_for_type_instantiation = false);
    void typecheck_function(Ast_Function *function);
};


#endif
