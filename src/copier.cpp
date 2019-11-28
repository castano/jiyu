
#include "copier.h"
#include "ast.h"
#include "compiler.h"
#include "sema.h"

#include <new> // for placement new

#define COPIER_NEW(type) ((type *)init_copy(new (compiler->get_memory(sizeof(type))) type(), old))

#define COPY_ARRAY(name) do { for (auto i : old->name) {_new->name.add((decltype(i))copy(i)); } } while (0) 
#define COPY_P(name)     ( _new->name = old->name )
#define COPY(name)       ( _new->name = (decltype(_new->name))copy(old->name) )

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
    
    currently_copying_function = outer_function;
    
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
            COPY(statement);
            
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
            COPY(typename_identifier);
            COPY(array_element_type);
            COPY(array_size_expression);
            COPY_P(array_is_dynamic);
            
            return _new;
        }
        case AST_TYPE_ALIAS: {
            auto old  = static_cast<Ast_Type_Alias *>(expression);
            auto _new = COPIER_NEW(Ast_Type_Alias);
            
            COPY(identifier);
            COPY(internal_type_inst);
            
            // @Incomplete I think, type_value can be set when internal_type_inst isnt, when the compiler creates a type alias during polymorphing,
            // do we ever need to handle that case here?
            if (!_new->internal_type_inst) assert(!_new->type_value);
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
        case AST_FOR: {
            auto old  = static_cast<Ast_For *>(expression);
            auto _new = COPIER_NEW(Ast_For);
            
            COPY(iterator_decl);
            COPY(iterator_index_decl);
            COPY(initial_iterator_expression);
            COPY(upper_range_expression);
            COPY_P(is_element_pointer_iteration);
            COPY_P(is_exclusive_end);
            
            // Dont do a copy here because the semantic pass will fil in the rest here.
            _new->iterator_declaration_scope.parent = get_current_scope();
            
            scope_stack.add(&_new->iterator_declaration_scope);
            copy_scope(&_new->body, &old->body);
            scope_stack.pop();
            return _new;
        }
        case AST_STRUCT: {
            auto old  = static_cast<Ast_Struct *>(expression);
            auto _new = COPIER_NEW(Ast_Struct);
            
            COPY(identifier);
            copy_scope(&_new->member_scope, &old->member_scope);
            
            return _new;
        }
        
        case AST_UNINITIALIZED:
        assert(false);
    }
    
    assert(false);
}

void Copier::copy_scope(Ast_Scope *_new, Ast_Scope *old) {
    _new->parent = get_current_scope();
    
    COPY_P(is_template_argument_block);
    
    scope_stack.add(_new);
    for (auto expr: old->statements) {
        auto stmt = copy(expr);
        if (stmt->type == AST_DECLARATION ||
            stmt->type == AST_FUNCTION    ||
            stmt->type == AST_TYPE_ALIAS  ||
            stmt->type == AST_STRUCT) {
            _new->declarations.add(stmt);
        }
        
        _new->statements.add(stmt);
    }
    scope_stack.pop();
}

Ast_Function *Copier::polymoprh_function_with_arguments(Ast_Function *poly, Array<Ast_Expression *> *arguments) {
    assert(arguments->count == poly->arguments.count);
    
    scope_stack.add(poly->polymorphic_type_alias_scope->parent);
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
    
    if (type_inst->typename_identifier) {
        // dont call this stuff here because we cant yet typecheck the resolved decl if it is
        // a template argument typealias
        // compiler->sema->typecheck_expression(type_inst->typename_identifier);
        
        auto decl = compiler->sema->find_declaration_for_atom(type_inst->typename_identifier->name, type_inst->typename_identifier->enclosing_scope);
        if (!decl) {
            compiler->report_error(type_inst->typename_identifier, "Undeclared identifier.\n");
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

