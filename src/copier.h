
#ifndef COPIER_H
#define COPIER_H

#include "general.h"

struct Compiler;
struct Ast_Scope;
struct Ast_Expression;
struct Ast_Function;
struct Ast_Type_Instantiation;

struct Ast_Type_Info;

struct Copier {
    Compiler *compiler;

    Array<Ast_Scope *> scope_stack;
    Ast_Function *currently_copying_function = nullptr;

    Copier(Compiler *compiler) {
        this->compiler = compiler;
    }

    Ast_Scope *get_current_scope();

    bool try_to_fill_polymorphic_type_aliases(Ast_Type_Instantiation *argument_type_inst, Ast_Type_Info *target_type_info, bool do_stuff_for_implicit_arg);
    Ast_Expression *copy(Ast_Expression *Ast_Expression);
    void copy_scope(Ast_Scope *_new, Ast_Scope *old);
    Ast_Function *copy_function(Ast_Function *old);
    Tuple<Ast_Function *, bool> polymoprh_function_with_arguments(Ast_Function *poly, Array<Ast_Expression *> *arguments, bool do_stuff_for_implicit_arg = false);
};

#endif
