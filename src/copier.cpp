
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
    COPY(return_decl);
    
    
    COPY(scope);
    
    scope_stack.pop();
    if (_new->polymorphic_type_alias_scope) scope_stack.pop();
    
    COPY_P(is_c_function);
    COPY_P(is_c_varargs);
    COPY_P(is_template_function);
    COPY_P(linkage_name);
    
    
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
            return _new;
        }
        case AST_UNARY_EXPRESSION: {
            auto old  = static_cast<Ast_Unary_Expression *>(expression);
            auto _new = COPIER_NEW(Ast_Unary_Expression);
            
            COPY_P(operator_type);
            COPY(expression);
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
            
            COPY(then_statement);
            COPY(else_statement);
            
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
            
            return _new;
        }
        case AST_SIZEOF: {
            auto old  = static_cast<Ast_Sizeof *>(expression);
            auto _new = COPIER_NEW(Ast_Sizeof);
            
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
            copy_scope(&_new->member_scope, &old->member_scope);
            _new->member_scope.owning_struct = _new;
            COPY_P(is_union);
            
            return _new;
        }

        case AST_ENUM: {
            auto old  = static_cast<Ast_Enum *>(expression);
            auto _new = COPIER_NEW(Ast_Enum);
            
            COPY(identifier);
            copy_scope(&_new->member_scope, &old->member_scope);
            _new->member_scope.owning_enum = _new;
            
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

Ast_Function *Copier::polymoprh_function_with_arguments(Ast_Function *poly, Array<Ast_Expression *> *arguments) {
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
        bool success = try_to_fill_polymorphic_type_aliases(arg_type_inst, target_type_info);
        
        if (!success) break;
    }
    
    scope_stack.pop();
    
    for (auto _alias: poly_copy->polymorphic_type_alias_scope->declarations) {
        auto alias = static_cast<Ast_Type_Alias *>(_alias);
        if (!alias->type_value) {
            String name = alias->identifier->name->name;
            compiler->report_error(alias, "Could not fill typealias '%.*s'.\n", name.length, name.data);
            return nullptr;
        }
    }
    
    return poly_copy;
}

bool Copier::try_to_fill_polymorphic_type_aliases(Ast_Type_Instantiation *type_inst, Ast_Type_Info *target_type_info) {    
    if (type_inst->pointer_to) {
        if (target_type_info->type != Ast_Type_Info::POINTER) return false;
        
        return try_to_fill_polymorphic_type_aliases(type_inst->pointer_to, target_type_info->pointer_to);
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
            return types_match(_struct->type_value, target_type_info);
        } else {
            assert(false);
        }
    }
    
    if (type_inst->array_element_type) {
        if (target_type_info->type != Ast_Type_Info::ARRAY) return false;
        
        // we dont worry about the size or anything here becuase our main goal if filling in the typealiases in the template parameter block
        bool success = try_to_fill_polymorphic_type_aliases(type_inst->array_element_type, target_type_info->array_element);
        if (compiler->errors_reported) return false;
        
        return success;
    }
    
    assert(false);
    return false;
}

Ast_Scope *Copier::get_current_scope() {
    return scope_stack[scope_stack.count-1];
}

