
#include "copier.h"
#include "ast.h"
#include "compiler.h"
#include "sema.h"

#include <new> // for placement new

#define COPIER_NEW(type) ((type *)init_copy(new (compiler->get_memory(sizeof(type))) type(), old))

#define COPY_ARRAY(name)   do { for (auto i : old->name) {_new->name.add((decltype(i))copy(i)); } } while (0)
#define COPY_ARRAY_P(name) do { for (auto i : old->name) {_new->name.add((decltype(i)) i);      } } while (0)
#define COPY_P(name)     ( _new->name = old->name )
#define COPY(name)       ( _new->name = (decltype(_new->name))copy(old->name) )

// :NoCopyForTypecheckedExpressions:
// In general, this stuff exists for creating polymorphs.
// Some AST-nodes can be readily copied because they dont rely
// on some known-external environment to be copied properly.
// For example, Ast_Literal can be copied freely, but Ast_Identifier
// cannot because it relies on it's position in the tree of scopes
// to be correct in order for lookups to work properly. Sema may also
// transform AST-nodes in ways that would make copying them properly more
// complicated than it should be. For example, Ast_Function_Call may or
// may not have a different list of arguments after typechecking due to
// implicit_argument_inserted. It may make sense to just handle this case,
// but I have chosen not to do that at this time.

// If in doubt about what should or should not be copied, consult the parser;
// If the parser creates and/or sets the data in question, then it should be
// copied. If the data is set by Sema then it likely shouldn't be copied.
// -josh 28 November 2019

static
Ast *init_copy(Ast* _new, Ast *old) {
    _new->text_span = old->text_span;
    _new->filename = old->filename;
    return _new;
}

Ast_Function *Copier::copy_function(Ast_Function *old) {
    Ast_Function *_new = COPIER_NEW(Ast_Function);

    auto outer_function = currently_copying_function;
    currently_copying_function = _new;
    defer { currently_copying_function = outer_function; };

    COPY(identifier);

    COPY(polymorphic_type_alias_scope);
    if (_new->polymorphic_type_alias_scope) {
        COPY_ARRAY(polymorphic_type_alias_scope->declarations);
        scope_stack.add(_new->polymorphic_type_alias_scope);
    }

    copy_scope(&_new->arguments_scope, &old->arguments_scope);
    scope_stack.add(&_new->arguments_scope);
    COPY_ARRAY(arguments);
    for (auto a: _new->arguments) {
        _new->arguments_scope.declarations.add(a);
    }
    COPY(return_type);


    COPY(scope);

    scope_stack.pop();
    if (_new->polymorphic_type_alias_scope) scope_stack.pop();

    COPY_P(is_c_function);
    COPY_P(is_c_varargs);
    COPY_P(is_template_function);
    COPY_P(linkage_name);
    COPY_P(is_operator_function);
    COPY_P(is_intrinsic);
    COPY_P(operator_type);


    return _new;
}

Ast_Expression *Copier::copy(Ast_Expression *expression) {
    if (!expression) return nullptr;

    switch (expression->type) {
        case AST_BINARY_EXPRESSION: {
            auto old  = static_cast<Ast_Binary_Expression *>(expression);
            auto _new = COPIER_NEW(Ast_Binary_Expression);

            COPY_P(operator_type);
            COPY(left);
            COPY(right);

            _new->enclosing_scope = get_current_scope();

            return _new;
        }
        case AST_UNARY_EXPRESSION: {
            auto old  = static_cast<Ast_Unary_Expression *>(expression);
            auto _new = COPIER_NEW(Ast_Unary_Expression);

            COPY_P(operator_type);
            COPY(expression);
            _new->enclosing_scope = get_current_scope();

            return _new;
        }
        case AST_IDENTIFIER: {
            auto old  = static_cast<Ast_Identifier *>(expression);
            auto _new = COPIER_NEW(Ast_Identifier);

            COPY_P(name);

            // This does not work in the general case.
            // This will probably work fine for polymorphing though.
            _new->enclosing_scope = get_current_scope();
            return _new;
        }
        case AST_DECLARATION: {
            auto old  = static_cast<Ast_Declaration *>(expression);
            auto _new = COPIER_NEW(Ast_Declaration);

            COPY(identifier);
            COPY(initializer_expression);
            COPY(type_inst);
            COPY_P(is_let);
            COPY_P(is_readonly_variable);
            COPY_P(is_struct_member);
            return _new;
        }
        case AST_SCOPE: {
            auto old = static_cast<Ast_Scope *>(expression);
            auto _new = COPIER_NEW(Ast_Scope);

            copy_scope(_new, old);
            return _new;
        }
        case AST_FUNCTION: {
            return copy_function(static_cast<Ast_Function *>(expression));
        }
        case AST_LITERAL: {
            auto old  = static_cast<Ast_Literal *>(expression);
            auto _new = COPIER_NEW(Ast_Literal);

            COPY_P(literal_type);

            switch (_new->literal_type) {
                case Ast_Literal::Type::INTEGER: COPY_P(integer_value); break;
                case Ast_Literal::Type::STRING:  COPY_P(string_value);  break;
                case Ast_Literal::Type::FLOAT:   COPY_P(float_value);   break;
                case Ast_Literal::Type::BOOL:    COPY_P(bool_value);    break;
                case Ast_Literal::Type::NULLPTR: break;
            }

            return _new;
        }
        case AST_FUNCTION_CALL: {
            auto old  = static_cast<Ast_Function_Call *>(expression);
            auto _new = COPIER_NEW(Ast_Function_Call);

            COPY(function_or_function_ptr);
            COPY_ARRAY(argument_list);

            // :NoCopyForTypecheckedExpressions:
            assert(old->implicit_argument_inserted == false);

            return _new;
        }
        case AST_DEREFERENCE: {
            auto old  = static_cast<Ast_Dereference *>(expression);
            auto _new = COPIER_NEW(Ast_Dereference);

            COPY(left);
            COPY(field_selector);

            return _new;
        }
        case AST_CAST: {
            auto old  = static_cast<Ast_Cast *>(expression);
            auto _new = COPIER_NEW(Ast_Cast);

            COPY(target_type_inst);
            COPY(expression);

            return _new;
        }
        case AST_IF: {
            auto old  = static_cast<Ast_If *>(expression);
            auto _new = COPIER_NEW(Ast_If);

            COPY(condition);

            copy_scope(&_new->then_scope, &old->then_scope);
            COPY(else_scope);

            return _new;
        }
        case AST_WHILE: {
            auto old  = static_cast<Ast_While *>(expression);
            auto _new = COPIER_NEW(Ast_While);

            COPY(condition);

            copy_scope(&_new->body, &old->body);
            _new->body.owning_statement = _new;
            return _new;
        }
        case AST_RETURN: {
            auto old  = static_cast<Ast_Return *>(expression);
            auto _new = COPIER_NEW(Ast_Return);

            _new->owning_function = currently_copying_function;
            COPY(expression);
            return _new;
        }
        case AST_TYPE_INSTANTIATION: {
            auto old  = static_cast<Ast_Type_Instantiation *>(expression);
            auto _new = COPIER_NEW(Ast_Type_Instantiation);

            COPY_P(builtin_primitive);
            COPY(pointer_to);
            COPY(type_dereference_expression);
            COPY(array_element_type);
            COPY(array_size_expression);
            COPY_P(array_is_dynamic);
            COPY(function_header);
            COPY(template_type_inst_of);
            COPY_ARRAY(template_type_arguments);

            return _new;
        }
        case AST_TYPE_ALIAS: {
            auto old  = static_cast<Ast_Type_Alias *>(expression);
            auto _new = COPIER_NEW(Ast_Type_Alias);

            COPY(identifier);
            COPY(internal_type_inst);

            // I think, type_value can be set when internal_type_inst isnt, when the compiler creates a type alias during polymorphing,
            // do we ever need to handle that case here?
            // if (!old->internal_type_inst) assert(!old->type_value);

            // After thinking about it, I think lite-copying type_value is fine.
            COPY_P(type_value);
            return _new;
        }
        case AST_ARRAY_DEREFERENCE: {
            auto old  = static_cast<Ast_Array_Dereference *>(expression);
            auto _new = COPIER_NEW(Ast_Array_Dereference);

            COPY(array_or_pointer_expression);
            COPY(index_expression);

            _new->enclosing_scope = get_current_scope();

            return _new;
        }
        case AST_SIZEOF: {
            auto old  = static_cast<Ast_Sizeof *>(expression);
            auto _new = COPIER_NEW(Ast_Sizeof);

            COPY_P(operator_type);
            COPY(target_type_inst);

            return _new;
        }
        case AST_TYPEOF: {
            auto old  = static_cast<Ast_Typeof *>(expression);
            auto _new = COPIER_NEW(Ast_Typeof);
            
            COPY(expression);
            
            return _new;
        }
        case AST_FOR: {
            auto old  = static_cast<Ast_For *>(expression);
            auto _new = COPIER_NEW(Ast_For);

            COPY_P(is_element_pointer_iteration);
            COPY_P(is_exclusive_end);
            COPY(iterator_decl);
            COPY(iterator_index_decl);
            COPY(initial_iterator_expression);
            COPY(upper_range_expression);

            // Dont do a copy here because the semantic pass will fil in the rest here.
            _new->iterator_declaration_scope.parent = get_current_scope();

            scope_stack.add(&_new->iterator_declaration_scope);
            copy_scope(&_new->body, &old->body);
            _new->body.owning_statement = _new;
            scope_stack.pop();
            return _new;
        }
        case AST_STRUCT: {
            auto old  = static_cast<Ast_Struct *>(expression);
            auto _new = COPIER_NEW(Ast_Struct);

            COPY(identifier);

            COPY(polymorphic_type_alias_scope);
            if (_new->polymorphic_type_alias_scope) {
                COPY_ARRAY(polymorphic_type_alias_scope->declarations);
                scope_stack.add(_new->polymorphic_type_alias_scope);
            }

            copy_scope(&_new->member_scope, &old->member_scope);

            if (_new->polymorphic_type_alias_scope) scope_stack.pop();

            _new->member_scope.owning_struct = _new;
            COPY_P(is_union);
            COPY_P(is_tuple);
            COPY_P(is_template_struct);
            COPY_P(is_anonymous);
            COPY_P(polymorph_source_struct); // Light copy; I am not totally sure this is the right thing to do...

            return _new;
        }

        case AST_ENUM: {
            auto old  = static_cast<Ast_Enum *>(expression);
            auto _new = COPIER_NEW(Ast_Enum);
            
            COPY(identifier);
            copy_scope(&_new->member_scope, &old->member_scope);
            _new->member_scope.owning_enum = _new;
            COPY_P(is_flags);
            
            // @@ Do we need to copy anything else?

            return _new;
        }

        // These guys do not get copied at all. They are resolved by the compiler after parsing irregardless of
        // the fact that these may be declared in templates.
        case AST_DIRECTIVE_LOAD:
        case AST_DIRECTIVE_IMPORT:
        case AST_DIRECTIVE_STATIC_IF:
            return expression;

        case AST_SCOPE_EXPANSION: {
            auto old = static_cast<Ast_Scope_Expansion *>(expression);
            auto _new = COPIER_NEW(Ast_Scope_Expansion);

            if (!old->expanded_via_import_directive) {
                copy_scope(_new->scope, old->scope);
            } else {
                COPY_P(scope); // Just do a lite-copy since we need this for scope lookups.
            }

            return _new;
        }

        case AST_OS: {
            auto old  = static_cast<Ast_Os *>(expression);
            auto _new = COPIER_NEW(Ast_Os);

            COPY(expression);
            return _new;
        }
        case AST_LIBRARY: {
            auto old  = static_cast<Ast_Library *>(expression);
            auto _new = COPIER_NEW(Ast_Library);

            COPY_P(is_framework);
            COPY_P(libname);
            return _new;
        }
        case AST_CONTROL_FLOW: {
            auto old  = static_cast<Ast_Control_Flow *>(expression);
            auto _new = COPIER_NEW(Ast_Control_Flow);

            COPY_P(control_type);
            _new->current_scope = get_current_scope();

            return _new;
        }

        case AST_TUPLE_EXPRESSION: {
            auto old  = static_cast<Ast_Tuple_Expression *>(expression);
            auto _new = COPIER_NEW(Ast_Tuple_Expression);

            COPY_ARRAY(arguments);

            return _new;
        }

        case AST_UNINITIALIZED:
        default:
            assert(false);
            return nullptr;

    }
}

void Copier::copy_scope(Ast_Scope *_new, Ast_Scope *old) {
    _new->parent = get_current_scope();

    COPY_P(is_template_argument_block);
    COPY_P(rejected_by_static_if);

    // We dont do a copy for owning_function because it would have been set by copy_function.

    scope_stack.add(_new);
    for (auto expr: old->statements) {
        auto stmt = copy(expr);
        if (is_declaration(stmt->type)) {
            _new->declarations.add(stmt);
        }

        _new->statements.add(stmt);
    }

    COPY_ARRAY_P(private_declarations);

    scope_stack.pop();
}

Tuple<Ast_Function *, bool> Copier::polymorph_function_with_arguments(Ast_Function *poly, Array<Ast_Expression *> *arguments, bool do_stuff_for_implicit_arg, Ast *callsite, bool allow_errors) {
    assert(arguments->count == poly->arguments.count);

    scope_stack.add(poly->polymorphic_type_alias_scope->parent);
    // @Speed @Memory we shouldnt be doing a full function copy at this point, we only need the header information copied
    // until we actually know if that we can fill all type aliases.
    Ast_Function *poly_copy = copy_function(poly);
    poly_copy->is_template_function = false; // this is no longer a template

    for (array_count_type i = 0; i < arguments->count; ++i) {
        auto target_type_info = get_type_info((*arguments)[i]);
        assert(target_type_info);

        auto arg_type_inst = poly_copy->arguments[i]->type_inst;
        bool success = try_to_fill_polymorphic_type_aliases(arg_type_inst, target_type_info, do_stuff_for_implicit_arg && i == 0);

        if (!success) break;
    }

    scope_stack.pop();

    for (auto _alias: poly_copy->polymorphic_type_alias_scope->declarations) {
        auto alias = static_cast<Ast_Type_Alias *>(_alias);
        if (!alias->internal_type_inst && !alias->type_value) {
            String name = alias->identifier->name->name;
            if (allow_errors) {
                compiler->report_error(alias, "Could not fill typealias '%.*s'.\n", name.length, name.data);
                compiler->report_error(callsite, "From call site:\n");
            }
            return MakeTuple<Ast_Function *, bool>(nullptr, false);
        }
    }

    // If a polymorph of the same template arguments already exists, then drop this one and return the existing one..
    for (auto existing: poly->polymorphed_overloads) {
        auto &existing_decls = existing->polymorphic_type_alias_scope->declarations;
        auto &new_decls      = poly_copy->polymorphic_type_alias_scope->declarations;

        assert(new_decls.count == existing_decls.count);

        bool does_match = true;
        for (array_count_type i = 0; i < new_decls.count; ++i) {
            auto exist_ta = static_cast<Ast_Type_Alias *>(existing_decls[i]);
            auto new_ta = static_cast<Ast_Type_Alias *>(new_decls[i]);

            if (!types_match(exist_ta->type_value, new_ta->type_value)) {
                does_match = false;
                break;
            }
        }

        // @TODO maybe we can free the memory we created for this polymorph
        if (does_match) {
            return MakeTuple(existing, true);
        }
    }

    return MakeTuple(poly_copy, false);
}

// @Incomplete do_stuff_for_implicit_arg currently allows you to dereference a struct-or-array-type
// and automatically fill one level of indirection of a pointer, but it doesnt allow you to dereference
// a pointer-type and fill a struct-or-array-type argument.
bool Copier::try_to_fill_polymorphic_type_aliases(Ast_Type_Instantiation *type_inst, Ast_Type_Info *target_type_info, bool do_stuff_for_implicit_arg, bool is_for_filling_a_template_inst) {
    target_type_info = get_final_type(target_type_info);

    if (type_inst->pointer_to) {
        if (do_stuff_for_implicit_arg) {
            bool is_struct_or_array = is_struct_type(target_type_info) || is_array_type(target_type_info);
            if (is_struct_or_array) {
                return try_to_fill_polymorphic_type_aliases(type_inst->pointer_to, target_type_info, false);
            }
        }

        if (target_type_info->type != Ast_Type_Info::POINTER) return false;
        return try_to_fill_polymorphic_type_aliases(type_inst->pointer_to, target_type_info->pointer_to, false);
    }

    if (type_inst->builtin_primitive) {
        return types_match(type_inst->builtin_primitive, target_type_info);
    }

    if (type_inst->type_dereference_expression) {
        if (type_inst->type_dereference_expression->type != AST_IDENTIFIER) {
            // @TODO this should probably be done at parse time.
            compiler->report_error(type_inst->type_dereference_expression, "Type-name of polymorphic template argument must be an identifier.\n");
            return false;
        }
        // dont call this stuff here because we cant yet typecheck the resolved decl if it is
        // a template argument typealias
        // compiler->sema->typecheck_expression(type_inst->typename_identifier);
        auto ident = static_cast<Ast_Identifier *>(type_inst->type_dereference_expression);
        auto decl = compiler->sema->find_declaration_for_atom(ident->name, ident->enclosing_scope);
        if (!decl) {
            compiler->report_error(ident, "Undeclared identifier '%.*s'.\n", PRINT_ARG(ident->name->name));
            return false;
        }

        if (compiler->errors_reported) return false;

        if (decl->type == AST_TYPE_ALIAS) {
            auto alias = static_cast<Ast_Type_Alias *>(decl);
            if (!alias->internal_type_inst && !alias->type_value) {
                // template argument
                assert(target_type_info);

                if (is_for_filling_a_template_inst) {
                    auto target_struct = target_type_info->struct_decl;
                    if (target_struct && target_struct->polymorph_source_struct) {
                        compiler->report_error(ident, "Filling polymorphic template argument using an uninstantiated polymorphic type is unsupported.\n");
                        return false;
                        /*
                        auto old = alias;
                        auto type_inst = COPIER_NEW(Ast_Type_Instantiation);
                        type_inst->type_dereference_expression = target_struct->polymorph_source_struct;
                        alias->internal_type_inst = type_inst;
                        return true;
                        */
                    }
                }

                alias->type_value = target_type_info;
                return true;
            } else {
                compiler->sema->typecheck_expression(alias);
                if (compiler->errors_reported) return false;

                assert(alias->type_value);
                return types_match(alias->type_value, target_type_info);
            }
        } else if (decl->type == AST_STRUCT) {
            auto _struct = static_cast<Ast_Struct *>(decl);
            compiler->sema->typecheck_expression(_struct);

            if (_struct->is_template_struct) {
                auto target = target_type_info->struct_decl;
                if (!target) return false;;

                return _struct == target->polymorph_source_struct;
            }
            return types_match(_struct->type_value, target_type_info);
        } else {
            assert(false);
        }
    }

    if (type_inst->array_element_type) {
        if (target_type_info->type != Ast_Type_Info::ARRAY) return false;

        // we dont worry about the size or anything here becuase our main goal if filling in the typealiases in the template parameter block
        bool success = try_to_fill_polymorphic_type_aliases(type_inst->array_element_type, target_type_info->array_element, false);
        if (compiler->errors_reported) return false;

        return success;
    }

    if (type_inst->template_type_inst_of) {
        if (target_type_info->type != Ast_Type_Info::STRUCT) return false;

        auto struct_decl = target_type_info->struct_decl;
        if (!struct_decl->polymorphic_type_alias_scope) return false;

        bool success = try_to_fill_polymorphic_type_aliases(type_inst->template_type_inst_of, struct_decl->type_value, false, true);
        if (!success) return false;

        for (array_count_type i = 0; i < struct_decl->polymorphic_type_alias_scope->declarations.count; ++i) {
            auto alias = struct_decl->polymorphic_type_alias_scope->declarations[i];
            assert(alias->type == AST_TYPE_ALIAS);

            auto type_value = get_type_declaration_resolved_type(alias);
            assert(type_value);

            auto inst = type_inst->template_type_arguments[i];
            bool success = try_to_fill_polymorphic_type_aliases(inst, type_value, false);
            if (!success) return false;
        }

        return true;
    }

    if (type_inst->function_header) {
        if (target_type_info->type != Ast_Type_Info::FUNCTION) return false;

        bool success = try_to_fill_polymorphic_type_aliases(type_inst->function_header->return_type, target_type_info->return_type, false);
        if (!success || compiler->errors_reported) return false;

        auto argument_count = type_inst->function_header->arguments.count;
        if (argument_count != target_type_info->arguments.count) {
            return false; // @@ Output error?
        }

        for (array_count_type i = 0; i < argument_count; i++) {
            auto decl = type_inst->function_header->arguments[i];

            Ast_Type_Info * type_value = target_type_info->arguments[i];

            success = try_to_fill_polymorphic_type_aliases(decl->type_inst, type_value, false);
            if (!success) return false;
        }

        return true;
    }

    assert(false);
    return false;
}

Ast_Scope *Copier::get_current_scope() {
    return scope_stack[scope_stack.count-1];
}

