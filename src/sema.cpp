
#include "sema.h"
#include "ast.h"
#include "compiler.h"
#include "copier.h"

#include <stdio.h>
#include <new> // for placement new

#define SEMA_NEW(type) (new (compiler->get_memory(sizeof(type))) type())

void add_type(String_Builder *builder, Ast_Type_Info *type) {
    if (type->type == Ast_Type_Info::INTEGER) {
        if (type->is_signed) builder->putchar('s');
        else                 builder->putchar('u');
        
        auto size = type->size;
        assert(size == 1 || size == 2 || size == 4 || size == 8);
        builder->print("%d", size);
    } else if (type->type == Ast_Type_Info::FLOAT) {
        if      (type->size == 4) builder->putchar('f');
        else if (type->size == 8) builder->putchar('F');
        else assert(false);
    } else if (type->type == Ast_Type_Info::VOID) {
        builder->putchar('v');
    } else if (type->type == Ast_Type_Info::BOOL) {
        builder->putchar('b');
    } else if (type->type == Ast_Type_Info::STRING) {
        builder->putchar('s');
    } else if (type->type == Ast_Type_Info::POINTER) {
        builder->putchar('p');
        add_type(builder, type->pointer_to);
    } else if (type->type == Ast_Type_Info::ALIAS) {
        add_type(builder, type->alias_of);
    } else if (type->type == Ast_Type_Info::ARRAY) {
        builder->putchar('A');
        
        if (type->array_element_count >= 0) {
            builder->putchar('k');
            builder->print("%d", type->array_element_count);
        } else if (type->is_dynamic) {
            builder->putchar('d');
        } else {
            builder->putchar('s');
        }
        
        
        builder->putchar('_');
        add_type(builder, type->array_element);
        builder->putchar('_');
    } else if (type->type == Ast_Type_Info::STRUCT) {
        builder->putchar('S');
        
        // @Incomplete structs that are declared with other structs/named-scopes.
        // @Incomplete anonymous structs?
        String name = type->struct_decl->identifier->name->name;
        builder->print("%d%.*s", name.length, name.length, name.data);
    } else if (type->type == Ast_Type_Info::FUNCTION) {
        builder->putchar('L');

        builder->print("%d", type->arguments.count);
        for (auto arg: type->arguments) {
            add_type(builder, arg);
        }

        add_type(builder, type->return_type);
        builder->putchar('_');
        if (type->is_c_function) builder->putchar('C');
        if (type->is_c_function) builder->putchar('V');
        builder->putchar('_');
    } else {
        assert(false && "Internal error: unhandled type when creating function mangled name.");
    }
}

void maybe_add_parent_scope_name(String_Builder *builder, Ast_Scope *start) {
    if (start->parent) maybe_add_parent_scope_name(builder, start->parent);
    
    if (start->owning_struct) {
        auto _struct = start->owning_struct;
        
        if (_struct->identifier) {
            String name = _struct->identifier->name->name;
            
            builder->print("%d%.*s", name.length, name.length, name.data);
        }
    }
}

String get_mangled_name(Compiler *compiler, Ast_Function *function) {
    if (function->identifier->name == compiler->atom_main) return function->identifier->name->name;
    String_Builder builder;
    
    builder.append("_H");
    
    assert(function->scope);
    maybe_add_parent_scope_name(&builder, function->scope);
    
    String name = function->identifier->name->name;
    builder.print("%d%.*s", name.length, name.length, name.data);
    
    builder.putchar('_');
    
    for (auto arg: function->arguments) {
        auto type = get_type_info(arg);
        add_type(&builder, type);
    }
    
    return builder.to_string();
}

s32 get_levels_of_indirection(Ast_Type_Info *info) {
    s32 count = 0;
    
    while (info) {
        if (info->type == Ast_Type_Info::POINTER) {
            info = info->pointer_to;
            count++;
        } else {
            break;
        }
    }
    
    return count;
}

bool type_is_iterable(Ast_Type_Info *info) {
    if (info->type == Ast_Type_Info::ARRAY) return true;
    
    // @Incomplete test for structs containing .count member and supports [] overloading.
    
    return false;
}

bool type_points_to_void_eventually(Ast_Type_Info *ptr) {
    while (ptr) {
        if (ptr->type == Ast_Type_Info::POINTER) {
            ptr = ptr->pointer_to;
        } else {
            return ptr->type == Ast_Type_Info::VOID;
        }
    }
    
    return false;
}

// @Incomplete this should help build the fully-qualified name of the type we're talking about, not just walk up all the scopes (since the target struct may be declared within a function that is declared within a struct, which means, this info would be wrong). But perhaps, for error messages, we do want the complete tree of named scopes. Hmm...
void maybe_add_struct_parent_name(String_Builder *builder, Ast_Scope *start) {
    if (start->parent) maybe_add_struct_parent_name(builder, start->parent);
    
    if (start->owning_struct) {
        auto _struct = start->owning_struct;
        
        if (_struct->identifier) {
            String name = _struct->identifier->name->name;
            
            builder->print("%.*s.", name.length, name.data);
        }
    }
}

void print_type_to_builder(String_Builder *builder, Ast_Type_Info *info) {
    if (info->type == Ast_Type_Info::INTEGER) {
        if (info->is_signed) {
            switch (info->size) {
                case 1: builder->print("int8"); return;
                case 2: builder->print("int16"); return;
                case 4: builder->print("int32"); return;
                case 8: builder->print("int64"); return;
                default: assert(false);
            }
        } else {
            switch (info->size) {
                case 1: builder->print("uint8"); return;
                case 2: builder->print("uint16"); return;
                case 4: builder->print("uint32"); return;
                case 8: builder->print("uint64"); return;
                default: assert(false);
            }
        }
    }
    
    if (info->type == Ast_Type_Info::BOOL) {
        builder->print("bool");
        return;
    }
    
    if (info->type == Ast_Type_Info::FLOAT) {
        if (info->size == 4) {
            builder->print("float");
        } else {
            assert(info->size == 8);
            builder->print("double");
        }
        return;
    }
    
    if (info->type == Ast_Type_Info::POINTER) {
        builder->print("*");
        print_type_to_builder(builder, info->pointer_to);
        return;
    }
    
    if (info->type == Ast_Type_Info::STRING) {
        builder->print("string");
        return;
    }
    
    if (info->type == Ast_Type_Info::VOID) {
        builder->print("void");
        return;
    }
    
    if (info->type == Ast_Type_Info::ARRAY) {
        builder->print("[");
        
        if (info->is_dynamic) {
            builder->print("..");
        } else if (info->array_element_count >= 0) {
            builder->print("%d", info->array_element_count);
        }
        
        builder->print("] ");
        
        print_type_to_builder(builder, info->array_element);
        return;
    }
    
    if (info->type == Ast_Type_Info::STRUCT) {
        auto _struct = info->struct_decl;
        
        maybe_add_struct_parent_name(builder, _struct->member_scope.parent);
        
        if (_struct->identifier) {
            String name = _struct->identifier->name->name;
            builder->print("%.*s", name.length, name.data);
        }
        return;
    }
    
    assert(false);
}

String type_to_string(Ast_Type_Info *info) {
    String_Builder builder;
    print_type_to_builder(&builder, info);
    return builder.to_string();
}

Ast_Expression *cast_int_to_int(Compiler *compiler, Ast_Expression *expr, Ast_Type_Info *target) {
    while (expr->substitution) expr = expr->substitution;
    
    assert(expr->type_info->type == Ast_Type_Info::INTEGER);
    assert(target->type == Ast_Type_Info::INTEGER);
    
    if (target->size == expr->type_info->size) return expr;
    
    Ast_Cast *cast = SEMA_NEW(Ast_Cast);
    copy_location_info(cast, expr);
    cast->expression = expr;
    // cast->target_type_inst = nullptr;
    cast->type_info = target;
    return cast;
}

Ast_Expression *cast_float_to_float(Compiler *compiler, Ast_Expression *expr, Ast_Type_Info *target) {
    while (expr->substitution) expr = expr->substitution;
    
    assert(expr->type_info->type == Ast_Type_Info::FLOAT);
    assert(target->type == Ast_Type_Info::FLOAT);
    
    if (target->size == expr->type_info->size) return expr;
    
    Ast_Cast *cast = SEMA_NEW(Ast_Cast);
    copy_location_info(cast, expr);
    cast->expression = expr;
    // cast->target_type_info = nullptr;
    cast->type_info = target;
    return cast;
}

Ast_Expression *cast_int_to_float(Compiler *compiler, Ast_Expression *expr, Ast_Type_Info *target) {
    while (expr->substitution) expr = expr->substitution;
    
    assert(expr->type_info->type == Ast_Type_Info::INTEGER);
    assert(target->type == Ast_Type_Info::FLOAT);
    
    Ast_Cast *cast = SEMA_NEW(Ast_Cast);
    copy_location_info(cast, expr);
    cast->expression = expr;
    // cast->target_type_info = nullptr;
    cast->type_info = target;
    return cast;
    
}

Ast_Expression *cast_ptr_to_ptr(Compiler *compiler, Ast_Expression *expr, Ast_Type_Info *target) {
    while (expr->substitution) expr = expr->substitution;
    
    assert(expr->type_info->type == Ast_Type_Info::POINTER);
    assert(target->type == Ast_Type_Info::POINTER);
    
    Ast_Cast *cast = SEMA_NEW(Ast_Cast);
    copy_location_info(cast, expr);
    cast->expression = expr;
    cast->type_info = target;
    return cast;
}

Ast_Literal *make_string_literal(Compiler *compiler, String value, Ast *source_loc = nullptr) {
    Ast_Literal *lit = SEMA_NEW(Ast_Literal);
    lit->literal_type = Ast_Literal::STRING;
    lit->string_value = value;
    lit->type_info = compiler->type_string;
    
    if (source_loc) copy_location_info(lit, source_loc);
    return lit;
}

Ast_Literal *make_integer_literal(Compiler *compiler, s64 value, Ast_Type_Info *type_info, Ast *source_loc = nullptr) {
    Ast_Literal *lit = SEMA_NEW(Ast_Literal);
    lit->literal_type = Ast_Literal::INTEGER;
    lit->integer_value = value;
    lit->type_info = type_info;
    
    if (source_loc) copy_location_info(lit, source_loc);
    return lit;
}

Ast_Literal *make_float_literal(Compiler *compiler, double value, Ast_Type_Info *type_info, Ast *source_loc = nullptr) {
    Ast_Literal *lit = SEMA_NEW(Ast_Literal);
    lit->literal_type = Ast_Literal::FLOAT;
    lit->float_value = value;
    lit->type_info = type_info;
    
    if (source_loc) copy_location_info(lit, source_loc);
    return lit;
}

Ast_Literal *make_bool_literal(Compiler *compiler, bool value, Ast *source_loc = nullptr) {
    Ast_Literal *lit = SEMA_NEW(Ast_Literal);
    lit->literal_type = Ast_Literal::BOOL;
    lit->bool_value = value;
    lit->type_info = compiler->type_bool;
    
    if (source_loc) copy_location_info(lit, source_loc);
    return lit;
}

Ast_Literal *make_null_literal(Compiler *compiler, Ast_Type_Info *pointer_type, Ast *source_loc = nullptr) {
    Ast_Literal *lit = SEMA_NEW(Ast_Literal);
    lit->literal_type = Ast_Literal::NULLPTR;
    lit->type_info = pointer_type;
    
    if (source_loc) copy_location_info(lit, source_loc);
    return lit;
}


Ast_Identifier *make_identifier(Compiler *compiler, Atom *name) {
    Ast_Identifier *ident = SEMA_NEW(Ast_Identifier);
    ident->name = name;
    return ident;
}

// @Note MUST typecheck the return value of this!!!
Ast_Array_Dereference *make_array_index(Compiler *compiler, Ast_Expression *array, Ast_Expression *index) {
    Ast_Array_Dereference *deref = SEMA_NEW(Ast_Array_Dereference);
    deref->array_or_pointer_expression = array;
    deref->index_expression = index;
    return deref;
}

// @Note MUST typecheck the return value of this!!!
Ast_Dereference *make_dereference(Compiler *compiler, Ast_Expression *aggregate_expression, Atom *field) {
    auto ident = make_identifier(compiler, field);
    copy_location_info(ident, aggregate_expression);
    
    Ast_Dereference *deref = SEMA_NEW(Ast_Dereference);
    copy_location_info(deref, aggregate_expression);
    deref->left = aggregate_expression;
    deref->field_selector = ident;
    return deref;
}

// @Note MUST typecheck the return value of this!!!
Ast_Unary_Expression *make_unary(Compiler *compiler, Token::Type op, Ast_Expression *subexpr) {
    Ast_Unary_Expression *un = SEMA_NEW(Ast_Unary_Expression);
    un->operator_type = op;
    un->expression = subexpr;
    return un;
}

Ast_Binary_Expression *make_binary(Compiler *compiler, Token::Type op, Ast_Expression *left, Ast_Expression *right, Ast *location) {
    Ast_Binary_Expression *bin = SEMA_NEW(Ast_Binary_Expression);
    bin->operator_type = op;
    bin->left = left;
    bin->right = right;
    if (location) copy_location_info(bin, location);

    return bin;
}

bool expression_is_lvalue(Ast_Expression *expression, bool parent_wants_lvalue) {
    while (expression->substitution) expression = expression->substitution;
    
    switch (expression->type) {
        case AST_IDENTIFIER: {
            auto ident = static_cast<Ast_Identifier *>(expression);
            auto decl = static_cast<Ast_Declaration *>(ident->resolved_declaration);
            
            if (decl) assert(decl->type == AST_DECLARATION);
            
            if (decl && decl->is_let) {
                return false;
            }
            
            return decl != nullptr;
        }
        
        case AST_DEREFERENCE: {
            auto deref = static_cast<Ast_Dereference *>(expression);
            
            return expression_is_lvalue(deref->left, parent_wants_lvalue);
        }
        
        case AST_ARRAY_DEREFERENCE: {
            auto deref = static_cast<Ast_Dereference *>(expression);
            // @Incomplete this isnt true if array_or_pointer_expression is a literal.
            // but maybe that never happens here due to substitution ?
            
            return true;
        }
        
        case AST_UNARY_EXPRESSION: {
            auto un = static_cast<Ast_Unary_Expression *>(expression);
            if (un->operator_type == Token::STAR) {
                auto expr = expression_is_lvalue(un->expression, true);
                return expr; // I think this is correct, but I havent thought about it deeply -josh 18 April 2019
            } else if (un->operator_type == Token::DEREFERENCE_OR_SHIFT) {
                auto expr = expression_is_lvalue(un->expression, false);
                if (parent_wants_lvalue) return true;
                return expr; // I think this is correct, but I havent thought about it deeply -josh 18 April 2019
            } else {
                assert(false);
            }
        }
    }
    
    return false;
}


// @Returns viability contribution
u64 maybe_mutate_literal_to_type(Ast_Literal *lit, Ast_Type_Info *want_numeric_type) {
    u64 viability_score = 0;
    if (lit->literal_type == Ast_Literal::INTEGER) {
        if (want_numeric_type && (want_numeric_type->type == Ast_Type_Info::INTEGER || want_numeric_type->type == Ast_Type_Info::FLOAT)) {
            if (!types_match(get_type_info(lit), want_numeric_type)) {
                viability_score += 1;
            }
            auto old_type = lit->type_info;
            // @Incomplete check that number can fit in target type
            // @Incomplete cast to float if we have an int literal
            lit->type_info = want_numeric_type;
            
            
            // @Cleanup I'm mutating the literal for now, but this would be a good place to use substitution, I think
            // Or since literal ints are considered completely typeless up until this point, maybe this is the right thing to do
            if (want_numeric_type->type == Ast_Type_Info::FLOAT) {
                lit->literal_type = Ast_Literal::FLOAT;
                
                if (old_type->is_signed) {
                    lit->float_value = static_cast<double>(lit->integer_value);
                } else {
                    lit->float_value = static_cast<double>(static_cast<u64>(lit->integer_value));
                }
                viability_score += 1;
            }
        } else {
            // lit->type_info = compiler->type_int32;
        }
    }
    
    if (lit->literal_type == Ast_Literal::FLOAT) {
        if (want_numeric_type && want_numeric_type->type == Ast_Type_Info::FLOAT) {
            if (!types_match(get_type_info(lit), want_numeric_type)) viability_score += 1;
            lit->type_info = want_numeric_type;
        }
        //else lit->type_info = compiler->type_float64; // @TODO we should probably have a check that verifies if the literal can fit in a 32-bit float and then default to that.
    }

    return viability_score;
}

const u32 ALLOW_COERCE_TO_PTR_VOID = (1 << 0);
const u32 ALLOW_COERCE_TO_BOOL     = (1 << 1);

Tuple<u64, Ast_Expression *> Sema::typecheck_and_implicit_cast_single_expression(Ast_Expression *expression, Ast_Type_Info *target_type_info, u32 allow_flags) {
    typecheck_expression(expression, target_type_info);
    
    if (compiler->errors_reported) return MakeTuple<u64, Ast_Expression *>(0, nullptr);
    
    while (expression->substitution) expression = expression->substitution;
    
    u64  viability_score  = 0;

    if (auto lit = folds_to_literal(expression)) {
        auto score = maybe_mutate_literal_to_type(lit, target_type_info);
        expression = lit;
        viability_score += score;
    }

    auto rtype = get_type_info(expression);
    auto ltype = target_type_info;
    
    auto right = expression;
    
    if (!types_match(ltype, rtype)) {
        if (is_int_type(ltype) && is_int_type(rtype) && (ltype->is_signed == rtype->is_signed)) {
            if (ltype->size > rtype->size) {
                right = cast_int_to_int(compiler, right, ltype);
                viability_score += 1;
            }
        } else if (is_float_type(ltype) && is_float_type(rtype)) {
            if (ltype->size > rtype->size) {
                right = cast_float_to_float(compiler, right, ltype);
                viability_score += 1;
            }
        } else if (is_float_type(ltype) && is_int_type(rtype)) {
            right = cast_int_to_float(compiler, right, ltype);
            viability_score += 10;
        } else if ((allow_flags & ALLOW_COERCE_TO_PTR_VOID) && is_pointer_type(ltype) && is_pointer_type(rtype)) {
            
            // @Note you're only allowed to coerce right-to-left here, meaning if the right-expression is *void, the left-expression cannot coerce away from whatever ptr type it is.
            if (type_points_to_void_eventually(ltype)) {
                auto left_indir = get_levels_of_indirection(ltype);
                auto right_indir = get_levels_of_indirection(rtype);
                
                if (left_indir == right_indir) {
                    right = cast_ptr_to_ptr(compiler, right, ltype);
                }
            }
            
            viability_score += 1;
        } else if (ltype->type == Ast_Type_Info::BOOL && (allow_flags & ALLOW_COERCE_TO_BOOL)) {
            if (is_pointer_type(rtype)) {
                auto null_expression = make_null_literal(compiler, rtype, /*location=*/expression);
                auto not_equal_null = make_binary(compiler, Token::NE_OP, expression, null_expression, /*location=*/expression);
                // @Speed we can probably just assign not_equal_null->type_info to ltype here...
                typecheck_expression(not_equal_null);
                
                right = not_equal_null;
                viability_score += 1;
            } else if (is_int_type(rtype)) {
                auto zero_expression = make_integer_literal(compiler, 0, rtype, /*location=*/expression);
                auto not_equal_zero = make_binary(compiler, Token::NE_OP, expression, zero_expression, /*location=*/expression);
                // @Speed we can probably just assign not_equal_null->type_info to ltype here...
                typecheck_expression(not_equal_zero);
                
                right = not_equal_zero;
                viability_score += 1;
            } else if (rtype->type == Ast_Type_Info::STRING) {
                auto empty_string_expression = make_string_literal(compiler, String(), /*location=*/expression);
                auto not_equal_empty_string = make_binary(compiler, Token::NE_OP, expression, empty_string_expression, /*location=*/expression);
                // @Speed we can probably just assign not_equal_null->type_info to ltype/compiler->type_bool here...
                typecheck_expression(not_equal_empty_string);
                
                right = not_equal_empty_string;
                viability_score += 1;
            }
        } else if (is_pointer_type(ltype) && types_match(ltype->pointer_to, compiler->type_uint8) && types_match(rtype, compiler->type_string)) {
            // Implicity dereference string when the left-type is *uint8.
            // @TODO maybe have an allow parameter for this in case this should only be allowed in certain situations.

            // Only allow this for literals because this is a convinience for passing literal strings to C functions!
            if (auto literal = folds_to_literal(expression)) {
                auto deref = make_dereference(compiler, literal, compiler->atom_data);
                typecheck_expression(deref);

                right = deref;
                viability_score += 1;
            }
        }
    }
    
    return MakeTuple(viability_score, right);
}

#define FOLD_COMPARE(op, lhs, rhs, type_info, site)                      \
{                                                                        \
    if (type_info->is_signed) {                                          \
        return make_bool_literal(compiler, lhs op rhs, site); \
    } else {                                                             \
        u64 l = static_cast<u64>(lhs);                                   \
        u64 r = static_cast<u64>(rhs);                                   \
        return make_bool_literal(compiler, l op r, site);     \
    }                                                                    \
}



Ast_Literal *Sema::folds_to_literal(Ast_Expression *expression) {
    typecheck_expression(expression);
    if (compiler->errors_reported) return nullptr;
    
    while (expression->substitution) expression = expression->substitution;
    
    if (expression->type == AST_LITERAL) return static_cast<Ast_Literal *>(expression);
    
    switch (expression->type) {
        case AST_BINARY_EXPRESSION: {
            auto bin = static_cast<Ast_Binary_Expression *>(expression);
            assert(bin->operator_type != Token::EQUALS); // equals is not foldable and probably shouldnt come through here
            
            auto left = folds_to_literal(bin->left);
            auto right = folds_to_literal(bin->right);
            
            if (compiler->errors_reported) return nullptr;
            
            if (!left || !right) return nullptr;
            
            auto left_type = get_type_info(left);
            auto right_type = get_type_info(right);
            
            if (!types_match(left_type, right_type)) {
                return nullptr;
            }
            
            if (left_type->type == Ast_Type_Info::INTEGER) {
                s64 left_int  = left->integer_value;
                s64 right_int = right->integer_value;
                switch (bin->operator_type) {
                    // @Incomplete I think. Should we be casting back and forth between the appropriate types and s64/u64?
                    case Token::PLUS : return make_integer_literal(compiler, left_int + right_int, left_type, bin);
                    case Token::MINUS: return make_integer_literal(compiler, left_int - right_int, left_type, bin);
                    case Token::STAR : return make_integer_literal(compiler, left_int * right_int, left_type, bin);
                    case Token::SLASH: return make_integer_literal(compiler, left_int / right_int, left_type, bin);
                    case Token::VERTICAL_BAR: return make_integer_literal(compiler, left_int | right_int, left_type, bin);

                    case Token::DEREFERENCE_OR_SHIFT: return make_integer_literal(compiler, left_int << right_int, left_type, bin);
                    // @Incomplete right-shift actually relies heavily on signedness...
                    // case Token::RIGHT_SHIFT: return make_integer_literal(compiler, left_int >> right_int, left_type, bin);
                    
                    case Token::LE_OP: FOLD_COMPARE(<=, left_int, right_int, left_type, bin);
                    case Token::GE_OP: FOLD_COMPARE(>=, left_int, right_int, left_type, bin);
                    case Token::EQ_OP: FOLD_COMPARE(==, left_int, right_int, left_type, bin);
                    case Token::NE_OP: FOLD_COMPARE(!=, left_int, right_int, left_type, bin);
                    case Token::LEFT_ANGLE : FOLD_COMPARE(<,  left_int, right_int, left_type, bin);
                    case Token::RIGHT_ANGLE: FOLD_COMPARE(>,  left_int, right_int, left_type, bin);
                    
                    default: assert(false);
                }
            } else if (left_type->type == Ast_Type_Info::FLOAT) {
                double l = left->float_value;
                double r = right->float_value;
                
                switch (bin->operator_type) {
                    case Token::PLUS : return make_float_literal(compiler, l + r, left_type, bin);
                    case Token::MINUS: return make_float_literal(compiler, l - r, left_type, bin);
                    case Token::STAR : return make_float_literal(compiler, l * r, left_type, bin);
                    case Token::SLASH: return make_float_literal(compiler, l / r, left_type, bin);
                    
                    case Token::LE_OP: make_bool_literal(compiler, l <= r, bin);
                    case Token::GE_OP: make_bool_literal(compiler, l >= r, bin);
                    case Token::EQ_OP: make_bool_literal(compiler, l == r, bin);
                    case Token::NE_OP: make_bool_literal(compiler, l != r, bin);
                    case Token::LEFT_ANGLE : make_bool_literal(compiler, l <  r, bin);
                    case Token::RIGHT_ANGLE: make_bool_literal(compiler, l >  r, bin);
                    
                    default: assert(false);
                }
            } else {
                return nullptr;
            }
        }
        
        case AST_UNARY_EXPRESSION: {
            auto un = static_cast<Ast_Unary_Expression *>(expression);
            
            auto rhs = folds_to_literal(un->expression);
            if (compiler->errors_reported) return nullptr;
            if (!rhs) return nullptr;
            
            if (un->operator_type == Token::MINUS) {
                auto left_type = get_type_info(un);
                
                assert(types_match(left_type, get_type_info(rhs)));
                if (left_type->type == Ast_Type_Info::INTEGER) {
                    assert(rhs->literal_type == Ast_Literal::INTEGER);
                    // @Incomplete if we need to work about casting between the target type sizes and s64, the we probably need to do that here too.
                    return make_integer_literal(compiler, -rhs->integer_value, left_type, un);
                } else if (left_type->type == Ast_Type_Info::FLOAT) {
                    assert(rhs->literal_type == Ast_Literal::FLOAT);
                    return make_float_literal(compiler, -rhs->float_value, left_type, un);
                } else {
                    return nullptr;
                }
            }
            
            return nullptr;
        }

        case AST_IDENTIFIER: {
            auto ident = static_cast<Ast_Identifier *>(expression);
            auto decl = static_cast<Ast_Declaration *>(ident->resolved_declaration);

            if (decl->type == AST_DECLARATION) {
                return folds_to_literal(decl);
            }

            return nullptr;
        }

        case AST_DECLARATION: {
            auto decl = static_cast<Ast_Declaration *>(expression);
            // @Incomplete should we typecheck decl first?
            assert(get_type_info(decl));

            if (decl->is_let && !decl->is_readonly_variable && decl->initializer_expression) {
                auto literal = folds_to_literal(decl->initializer_expression);
                if (literal) {
                    assert(literal->type == AST_LITERAL);
                    literal = static_cast<Ast_Literal *>(compiler->copier->copy(literal)); // Make a copy in case the user code wants to mutate the literal itself.
                    assert(literal);
                    literal->type_info = get_type_info(decl->initializer_expression);
                }
                return literal;
            }

            return nullptr;
        }
        
        default: return nullptr;
    }
}

// @Cleanup if we introduce expression substitution, then we can remove the resut parameters and just set (left/right)->substitution
// Actually, if we do that, then we can't really use this for checking function calls.
// Actually, this is being deprecated for things other than binary operators and for-loop ranges... since function calls now use typecheck_and_implicit_cast_single_expression, declarations need only do one-way casting, returns are one-way.
// Perhaps also, we should break this out into several calls to typecheck_and_implicit_cast_single_expression... -josh 21 July 2019
Tuple<u64, u64> Sema::typecheck_and_implicit_cast_expression_pair(Ast_Expression *left, Ast_Expression *right, Ast_Expression **result_left, Ast_Expression **result_right, bool allow_coerce_to_ptr_void) {
    if (auto lit = folds_to_literal(left)) {
        typecheck_expression(right);
        if (compiler->errors_reported) return MakeTuple<u64, u64>(0, 0);
        
        // typecheck_expression(left, get_type_info(right));
        assert(get_type_info(lit));
        maybe_mutate_literal_to_type(lit, get_type_info(right));
        left = lit;
    } else {
        typecheck_expression(left);
        if (compiler->errors_reported) return MakeTuple<u64, u64>(0, 0);
        
        typecheck_expression(right, get_type_info(left));
    }
    
    if (compiler->errors_reported) return MakeTuple<u64, u64>(0, 0);
    
    while (left->substitution)  left  = left->substitution;
    while (right->substitution) right = right->substitution;
    
    assert(left->type_info);
    assert(right->type_info);
    
    auto ltype = left->type_info;
    auto rtype = right->type_info;
    u64  left_viability_score  = 0;
    u64  right_viability_score = 0;
    
    if (!types_match(ltype, rtype)) {
        if (is_int_type(ltype) && is_int_type(rtype) && (ltype->is_signed == rtype->is_signed)) {
            if (ltype->size < rtype->size) {
                left = cast_int_to_int(compiler, left, rtype);
                left_viability_score += 1;
            } else if (ltype->size > rtype->size) {
                right = cast_int_to_int(compiler, right, ltype);
                right_viability_score += 1;
            }
        } else if (is_float_type(ltype) && is_float_type(rtype)) {
            if (ltype->size < rtype->size) {
                left = cast_float_to_float(compiler, left, rtype);
                left_viability_score += 1;
            } else if (ltype->size > rtype->size) {
                right = cast_float_to_float(compiler, right, ltype);
                right_viability_score += 1;
            }
        } else if (is_float_type(ltype) && is_int_type(rtype)) {
            right = cast_int_to_float(compiler, right, ltype);
            right_viability_score += 10;
        } else if (is_int_type(ltype) && is_float_type(rtype)) {
            left = cast_int_to_float(compiler, left, rtype);
            left_viability_score += 10;
        }else if (allow_coerce_to_ptr_void && is_pointer_type(ltype) && is_pointer_type(rtype)) {
            
            // @Note you're only allowed to coerce right-to-left here, meaning if the right-expression is *void, the left-expression cannot coerce away from whatever ptr type it is.
            if (type_points_to_void_eventually(ltype)) {
                auto left_indir = get_levels_of_indirection(ltype);
                auto right_indir = get_levels_of_indirection(rtype);
                
                if (left_indir == right_indir) {
                    right = cast_ptr_to_ptr(compiler, right, ltype);
                }
            }
            
            right_viability_score += 1;
        }
    }
    
    if (result_left)  *result_left  = left;
    if (result_right) *result_right = right;
    
    return MakeTuple(left_viability_score, right_viability_score);
}

void Sema::typecheck_scope(Ast_Scope *scope) {
    assert(scope->substitution == nullptr);
    
    for (auto &it : scope->statements) {
        // @TODO should we do replacements at the scope level?
        typecheck_expression(it);
    }
}

Ast_Function *Sema::get_polymorph_for_function_call(Ast_Function *template_function, Ast_Function_Call *call) {
    assert(template_function->is_template_function);
    
    // @Incomplete if we end up supporting varargs for native functions, then this needs to change
    if (call->argument_list.count != template_function->arguments.count) {
        return nullptr;
    }
    
    for (auto expr: call->argument_list) {
        typecheck_expression(expr);
        
        if (compiler->errors_reported) return nullptr;
    }
    
    for (auto overload: template_function->polymorphed_overloads) {
        assert(overload->arguments.count == call->argument_list.count);
        
        bool does_match = true;
        for (array_count_type i = 0; i < overload->arguments.count; ++i) {
            auto arg_type = get_type_info(overload->arguments[i]);
            auto call_type = get_type_info(call->argument_list[i]);
            
            assert(arg_type);
            assert(call_type);
            
            if (!types_match(arg_type, call_type)) {
                does_match = false;
                break;
            }
        }
        
        if (does_match) return overload;
    }
    
    // @Incomplete
    // from here we need to make a copy of the template
    // and then attempt to resolve the types of the function arguments
    // and resolve the targets of the template type aliases
    
    auto polymorph = compiler->copier->polymoprh_function_with_arguments(template_function, &call->argument_list);
    if (polymorph) {
        typecheck_function(polymorph);
        template_function->polymorphed_overloads.add(polymorph);
    }
    if (compiler->errors_reported) return nullptr;
    
    assert(!polymorph->is_template_function);
    return polymorph;
}

Tuple<bool, u64> Sema::function_call_is_viable(Ast_Function_Call *call, Ast_Type_Info *function_type, bool perform_full_check) {
    assert(function_type);
    assert(function_type->type == Ast_Type_Info::FUNCTION);
    
    bool pass_c_varags = (function_type->is_c_varargs && call->argument_list.count >= function_type->arguments.count);
    if (!pass_c_varags && call->argument_list.count != function_type->arguments.count) {
        // @TODO print function declaration as well as call site
        if (perform_full_check) {
            compiler->report_error(call, "Mismatch in function call arguments. Wanted %lld, got %lld.\n", function_type->arguments.count, call->argument_list.count);
        }
        return MakeTuple<bool, u64>(false, 0);
    }
    
    u64 viability_score = 0;
    for (array_count_type i = 0; i < call->argument_list.count; ++i) {
        auto original_value = call->argument_list[i];
        auto value = original_value;

        if (i < function_type->arguments.count) {
            // use null for morphed param because we don't care about the modified value because function parameter declarations can't be mutated here
            auto param_type = function_type->arguments[i];
            auto tuple = typecheck_and_implicit_cast_single_expression(value, param_type, ALLOW_COERCE_TO_PTR_VOID);
            u64 right_viability_score = tuple.item1;
            
            if (compiler->errors_reported) return MakeTuple<bool, u64>(false, 0);
            
            value = tuple.item2;
            auto value_type = get_type_info(value);
            if (!types_match(value_type, param_type)) {
                if (perform_full_check) {
                    auto wanted = type_to_string(param_type);
                    auto given  = type_to_string(value_type);
                    compiler->report_error(original_value, "Mismatch in function call argument types. (Wanted %.*s, Given %.*s).\n",
                                           wanted.length, wanted.data, given.length, given.data);
                    
                    free(wanted.data);
                    free(given.data);
                }
                return MakeTuple<bool, u64>(false, 0);
            }
            
            // if value was mutated away from argument_list then add to the score
            viability_score += right_viability_score;
        } else if (function_type->is_c_varargs) {
            // just do a normal typecheck on the call argument since this is for varargs
            typecheck_expression(value);
            viability_score += 1;
        } else {
            assert(false);
        }
        
        // set value into the list in case it got implicitly cast
        if (perform_full_check) call->argument_list[i] = value;
    }
    
    return MakeTuple<bool, u64>(true, viability_score);
}

void Sema::collect_function_overloads_for_atom_in_scope(Atom *atom, Ast_Scope *start, Array<Ast_Function *> *overload_set, bool check_private_declarations) {
    assert(start->rejected_by_static_if == false);
    for (auto it : start->declarations) {
        while (it->substitution) it = it->substitution;
        
        if (it->type == AST_FUNCTION) {
            auto function = static_cast<Ast_Function *>(it);
            if (function->identifier->name == atom) {
                // printf("Adding overlaod: %p\n", function);
                overload_set->add(function);
            }
        } else if (it->type == AST_SCOPE_EXPANSION) {
            auto exp = static_cast<Ast_Scope_Expansion *>(it);

            bool check_private = (exp->expanded_via_import_directive == nullptr);
            collect_function_overloads_for_atom_in_scope(atom, exp->scope, overload_set, check_private);
        }
    }

    if (check_private_declarations) {
        for (auto it : start->private_declarations) {
            while (it->substitution) it = it->substitution;
            
            if (it->type == AST_FUNCTION) {
                auto function = static_cast<Ast_Function *>(it);
                if (function->identifier->name == atom) {
                    // printf("Adding overlaod: %p\n", function);
                    overload_set->add(function);
                }
            } else if (it->type == AST_SCOPE_EXPANSION) {
                auto exp = static_cast<Ast_Scope_Expansion *>(it);

                bool check_private = (exp->expanded_via_import_directive == nullptr);
                collect_function_overloads_for_atom_in_scope(atom, exp->scope, overload_set, check_private);
            }
        }
    }
}

void Sema::collect_function_overloads_for_atom(Atom *atom, Ast_Scope *start, Array<Ast_Function *> *overload_set, bool check_private_declarations) {
    // printf("Start\n");
    while (start) {
        
        collect_function_overloads_for_atom_in_scope(atom, start, overload_set, check_private_declarations);
        
        start = start->parent;
        // printf("Ascending\n");
    }
}

Ast_Expression *Sema::find_declaration_for_atom_in_scope(Ast_Scope *scope, Atom *atom, bool check_private_declarations) {
    // @Incomplete check scope tree
    for (auto it : scope->declarations) {
        while (it->substitution) it = it->substitution;
        
        if (it->type == AST_DECLARATION) {
            auto decl = static_cast<Ast_Declaration *>(it);
            if (decl->identifier->name == atom) return it;
        } else if (it->type == AST_FUNCTION) {
            auto function = static_cast<Ast_Function *>(it);
            if (function->identifier->name == atom) return function;
        } else if (it->type == AST_TYPE_ALIAS) {
            auto alias = static_cast<Ast_Type_Alias *>(it);
            if (alias->identifier->name == atom) return alias;
        } else if (it->type == AST_STRUCT) {
            auto _struct = static_cast<Ast_Struct *>(it);
            if (_struct->identifier->name == atom) return _struct;
        } else if (it->type == AST_SCOPE_EXPANSION) {
            auto exp = static_cast<Ast_Scope_Expansion *>(it);

            bool check_private = (exp->expanded_via_import_directive == nullptr);
            auto decl = find_declaration_for_atom_in_scope(exp->scope, atom, check_private);
            if (decl) return decl;
        } else {
            assert(false);
        }
    }

    if (check_private_declarations) {
        for (auto it : scope->private_declarations) {
        while (it->substitution) it = it->substitution;
        
        if (it->type == AST_DECLARATION) {
            auto decl = static_cast<Ast_Declaration *>(it);
            if (decl->identifier->name == atom) return it;
        } else if (it->type == AST_FUNCTION) {
            auto function = static_cast<Ast_Function *>(it);
            if (function->identifier->name == atom) return function;
        } else if (it->type == AST_TYPE_ALIAS) {
            auto alias = static_cast<Ast_Type_Alias *>(it);
            if (alias->identifier->name == atom) return alias;
        } else if (it->type == AST_STRUCT) {
            auto _struct = static_cast<Ast_Struct *>(it);
            if (_struct->identifier->name == atom) return _struct;
        } else if (it->type == AST_SCOPE_EXPANSION) {
            auto exp = static_cast<Ast_Scope_Expansion *>(it);

            bool check_private = (exp->expanded_via_import_directive == nullptr);
            auto decl = find_declaration_for_atom_in_scope(exp->scope, atom, check_private);
            if (decl) return decl;
        } else {
            assert(false);
        }
    }
    }
    
    return nullptr;
}

Ast_Expression *Sema::find_declaration_for_atom(Atom *atom, Ast_Scope *start, bool check_private_declarations) {
    
    while (start) {
        auto decl = find_declaration_for_atom_in_scope(start, atom, check_private_declarations);
        if (decl) return decl;
        
        start = start->parent;
    }
    
    return nullptr;
}

s64 pad_to_alignment(s64 current, s64 align) {
    assert(align >= 1);
    
    s64 minum = current & (align-1);
    if (minum) {
        current += align - minum;
    }
    
    return current;
}

void Sema::typecheck_expression(Ast_Expression *expression, Ast_Type_Info *want_numeric_type, bool overload_set_allowed) {
    while (expression->substitution) expression = expression->substitution;

    // @Temporary maybe, if this is a function declaration, typecheck it anyways since typecheck_function_header() will have set
    // the type info on the function node, but not have checked the entire body.
    if (expression->type != AST_FUNCTION && expression->type_info) return;
    
    switch (expression->type) {
        case AST_DIRECTIVE_LOAD: {
            // @TODO Should we assert or error here if the directive has not yet been executed?
            expression->type_info = compiler->type_void;
            return;
        }
        case AST_DIRECTIVE_IMPORT: {
            // @TODO Should we assert or error here if the directive has not yet been executed?
            auto import = static_cast<Ast_Directive_Import *>(expression);
            expression->type_info = compiler->type_void;

            typecheck_scope(import->imported_scope);
            return;
        }
        case AST_DIRECTIVE_STATIC_IF: {
            expression->type_info = compiler->type_void;
            return;
        }
        
        case AST_SCOPE_EXPANSION: {
            auto exp = static_cast<Ast_Scope_Expansion *>(expression);
            exp->type_info = compiler->type_void;
            
            typecheck_scope(exp->scope);
            return;
        }
        
        case AST_IDENTIFIER: {
            auto ident = static_cast<Ast_Identifier *>(expression);
            assert(ident->name);
            
            auto decl = find_declaration_for_atom(ident->name, ident->enclosing_scope);
            
            if (!decl) {
                String name = ident->name->name;
                
                // @FixMe pass in ident
                compiler->report_error(ident, "Undeclared identifier '%.*s'\n", name.length, name.data);
            } else {
                if (decl->type == AST_FUNCTION) {
                    assert(ident->overload_set.count == 0);
                    collect_function_overloads_for_atom(ident->name, ident->enclosing_scope, &ident->overload_set);
                    
                    if (!overload_set_allowed && ident->overload_set.count > 1) {
                        String name = ident->name->name;
                        compiler->report_error(ident, "Ambiguous use of overloaded function '%.*s' (%d overloads).\n", name.length, name.data, ident->overload_set.count);
                        
                        
                        for (auto overload: ident->overload_set) {
                            compiler->report_error(overload, "DEBUG: here\n");
                        }
                        
                        return;
                    } else if (!overload_set_allowed) {
                        assert(ident->overload_set.count == 1);
                        
                        typecheck_expression(decl);
                        ident->resolved_declaration = decl;
                        ident->type_info = get_type_info(decl);
                        return;
                    }

                    // resolved_declaration and type_info will be resolved by the Ast_Function_Call code.
                    // Set to void for now, Ast_Function_Call code will either error or fix this up.
                    ident->type_info = compiler->type_void;
                } else {
                    typecheck_expression(decl);
                    ident->resolved_declaration = decl;
                    ident->type_info = get_type_info(decl);
                }
            }
            
            return;
        }
        
        case AST_DECLARATION: {
            auto decl = static_cast<Ast_Declaration *>(expression);
            
            if (decl->type_inst) {
                decl->type_info = resolve_type_inst(decl->type_inst);
                if (compiler->errors_reported) return;
            }
            
            // @TODO prevent use of a declaration in it's initializer
            if (decl->initializer_expression) {
                typecheck_expression(decl->initializer_expression, get_type_info(decl));
                if (compiler->errors_reported) return;
            }
            
            if (decl->is_let && !decl->is_readonly_variable && !decl->initializer_expression) {
                compiler->report_error(decl, "let constant must be initialized by an expression.\n");
            }
            
            if (decl->is_let && !decl->is_readonly_variable && decl->initializer_expression) {
                auto literal = folds_to_literal(decl->initializer_expression);

                if (!literal) {
                    compiler->report_error(decl->initializer_expression, "let constant may only be initialized by a literal expression.\n");
                } else {
                    if (decl->initializer_expression != literal) decl->initializer_expression->substitution = literal;
                }
                
                // decl->substitution = decl->initializer_expression;
            }
            
            if (!decl->is_let && decl->is_struct_member && decl->initializer_expression) {
                if (!resolves_to_literal_value(decl->initializer_expression)) {
                    compiler->report_error(decl->initializer_expression, "Struct member may only be initialized by a literal expression.\n");
                }
            }
            
            if (decl->type_inst) {
                // this should have already be resolved above
                assert(decl->type_info);
                // decl->type_info = resolve_type_inst(decl->type_inst);
            } else {
                assert(decl->initializer_expression);
                
                decl->type_info = get_type_info(decl->initializer_expression);
            }
            
            if (!decl->is_let && decl->identifier && compiler->is_toplevel_scope(decl->identifier->enclosing_scope)) {
                if (decl->initializer_expression && !resolves_to_literal_value(decl->initializer_expression)) {
                    compiler->report_error(decl, "Global variable may only be initialized by a literal expression.\n");
                }
                
                compiler->global_decl_emission_queue.add(decl);
            }

            if (compiler->errors_reported) return;
            
            if (decl->type_info && decl->initializer_expression) {
                auto result = typecheck_and_implicit_cast_single_expression(decl->initializer_expression, get_type_info(decl), ALLOW_COERCE_TO_PTR_VOID);
                
                if (decl->initializer_expression != result.item2) decl->initializer_expression = result.item2; // Don't do a substitution here, otherwise we can cause a loop
                if (!types_match(get_type_info(decl), get_type_info(decl->initializer_expression))) {
                    auto wanted = type_to_string(get_type_info(decl));
                    auto given  = type_to_string(get_type_info(decl->initializer_expression));
                    compiler->report_error(decl->initializer_expression, "Attempt to initialize variable with expression of incompatible type (Wanted %.*s, Given %.*s).\n",
                                           wanted.length, wanted.data, given.length, given.data);
                    free(wanted.data);
                    free(given.data);
                    return;
                }
            }
            
            return;
        }
        
        case AST_BINARY_EXPRESSION: {
            auto bin = static_cast<Ast_Binary_Expression *>(expression);

            if (bin->operator_type == Token::PLUS_EQ          ||      // +=
                bin->operator_type == Token::MINUS_EQ         ||      // -=
                bin->operator_type == Token::STAR_EQ          ||      // *=
                bin->operator_type == Token::SLASH_EQ         ||      // /=
                bin->operator_type == Token::PERCENT_EQ       ||      // %=
                bin->operator_type == Token::AMPERSAND_EQ     ||      // &=
                bin->operator_type == Token::VERTICAL_BAR_EQ  ||      // |=
                bin->operator_type == Token::CARET_EQ                 // ^=
                ) {
                // Desugar these shorthand operators.
                Token::Type op;
                switch (bin->operator_type) {
                    case Token::PLUS_EQ        : op = Token::PLUS; break;
                    case Token::MINUS_EQ       : op = Token::MINUS; break;
                    case Token::STAR_EQ        : op = Token::STAR; break;
                    case Token::SLASH_EQ       : op = Token::SLASH; break;
                    case Token::PERCENT_EQ     : op = Token::PERCENT; break;
                    case Token::AMPERSAND_EQ   : op = Token::AMPERSAND; break;
                    case Token::VERTICAL_BAR_EQ: op = Token::VERTICAL_BAR; break;
                    case Token::CARET_EQ       : op = Token::CARET; break;
                    default: assert(false);
                }

                auto operation = make_binary(compiler, op, bin->left, bin->right, bin);
                auto assignment = make_binary(compiler, Token::EQUALS, bin->left, operation, bin);

                bin->substitution = assignment;
                bin = assignment;
            }
            
            u32 allow_coerce_to_ptr_void_flag = ((bin->operator_type == Token::EQUALS) ? ALLOW_COERCE_TO_PTR_VOID : 0);
            
            if (bin->operator_type == Token::EQUALS) {
                // we're only allowed to cast on the rhs of an assignment.
                typecheck_expression(bin->left);
                if (compiler->errors_reported) return;
                
                auto tuple = typecheck_and_implicit_cast_single_expression(bin->right, get_type_info(bin->left), allow_coerce_to_ptr_void_flag);
                
                bin->right = tuple.item2;
            } else {
                // @Incomplete change typecheck_and_implicit_cast_expression_pair to use flags
                bool allow_coerce_to_ptr_void = (allow_coerce_to_ptr_void_flag & ALLOW_COERCE_TO_PTR_VOID);
                typecheck_and_implicit_cast_expression_pair(bin->left, bin->right, &bin->left, &bin->right, allow_coerce_to_ptr_void);
            }
            
            if (compiler->errors_reported) return;
            
            // @Hack @Incomplete
            bin->type_info = get_type_info(bin->left);
            
            assert(bin->type_info);
            
            
            if (bin->operator_type == Token::EQUALS) {
                if (!expression_is_lvalue(bin->left, true)) {
                    compiler->report_error(bin->left, "expression on lhs of '=' must be an lvalue.\n");
                }
            }
            
            if (bin->operator_type == Token::EQ_OP ||
                bin->operator_type == Token::NE_OP ||
                bin->operator_type == Token::LE_OP ||
                bin->operator_type == Token::GE_OP ||
                bin->operator_type == Token::RIGHT_ANGLE ||
                bin->operator_type == Token::LEFT_ANGLE ||
                bin->operator_type == Token::AND_OP ||
                bin->operator_type == Token::OR_OP) {
                bin->type_info = compiler->type_bool;
            }
            
            auto left_type  = get_type_info(bin->left);
            auto right_type = get_type_info(bin->right);
            
            if (bin->operator_type == Token::AND_OP ||
                bin->operator_type == Token::OR_OP) {
                if (left_type->type != Ast_Type_Info::BOOL) {
                    compiler->report_error(bin->left, "Left-hand side of boolean operator must be of type bool.\n");
                }
                
                if (right_type->type != Ast_Type_Info::BOOL) {
                    compiler->report_error(bin->right, "Right-hand side of boolean operator must be of type bool.\n");
                }
                
                if (compiler->errors_reported) return;
            }
            
            if (!types_match(left_type, right_type)) {
                if ((bin->operator_type == Token::PLUS
                     || bin->operator_type == Token::MINUS) &&
                    left_type->type == Ast_Type_Info::POINTER && right_type->type == Ast_Type_Info::INTEGER) {
                    return;
                }
                
                // @TOOD report operator
                auto lhs = type_to_string(left_type);
                auto rhs = type_to_string(right_type);
                compiler->report_error(bin, "Incompatible types found on lhs and rhs of binary operator (%.*s, %.*s).", lhs.length, lhs.data, rhs.length, rhs.data);
                free(lhs.data);
                free(rhs.data);
                return;
            }

            if (bin->operator_type == Token::LE_OP ||
                bin->operator_type == Token::GE_OP ||
                bin->operator_type == Token::RIGHT_ANGLE ||
                bin->operator_type == Token::LEFT_ANGLE) {
                if (!is_int_type(left_type) && !is_float_type(left_type)) {
                    compiler->report_error(bin, "Comparison operators are only valid for integer and floating-point operands.\n");
                    return;
                }
            }

            if (bin->operator_type == Token::EQ_OP || bin->operator_type == Token::NE_OP) {
                if (!is_int_type(left_type) && !is_float_type(left_type) &&
                    left_type->type != Ast_Type_Info::STRING && !is_pointer_type(left_type)
                    && left_type->type != Ast_Type_Info::BOOL && left_type->type != Ast_Type_Info::TYPE) {
                    compiler->report_error(bin, "Equal operator is only valid for integer, floating-point, pointer, string, and Type operands.\n");
                    return;
                }
            }

            if (bin->operator_type == Token::PLUS  ||
                bin->operator_type == Token::MINUS ||
                bin->operator_type == Token::SLASH ||
                bin->operator_type == Token::STAR) {
                bool pointer_arithmetic_allowed = (bin->operator_type == Token::MINUS) && is_pointer_type(left_type);
                if (!is_int_type(left_type) && !is_float_type(left_type) && !pointer_arithmetic_allowed) {
                    compiler->report_error(bin, "Arithmetic operators are only valid for integer and floating-point operands.\n");
                    return;
                }
            }

            if (bin->operator_type == Token::AMPERSAND    ||
                bin->operator_type == Token::VERTICAL_BAR ||
                bin->operator_type == Token::CARET) {
                if (!is_int_type(left_type)) {
                    compiler->report_error(bin, "Bitwise logical operators are only valid for integer operands.\n");
                    return;
                }
            }

            if (bin->operator_type == Token::DEREFERENCE_OR_SHIFT    ||
                bin->operator_type == Token::RIGHT_SHIFT) {
                if (!is_int_type(left_type)) {
                    compiler->report_error(bin, "Bitwise arithmetic operators are only valid for integer operands.\n");
                    return;
                }
            }

            if (bin->operator_type == Token::PERCENT) {
                if (!is_int_type(left_type) && !is_float_type(left_type)) {
                    compiler->report_error(bin, "Remainder operator is only valid for integer and floating-point operands.\n");
                    return;
                }
            }
            
            if ((bin->operator_type == Token::EQ_OP) || (bin->operator_type == Token::NE_OP)) {
                if (left_type->type == Ast_Type_Info::STRING) {
                    Ast_Function_Call *call = SEMA_NEW(Ast_Function_Call);
                    copy_location_info(call, bin);

                    auto identifier = make_identifier(compiler, compiler->atom___strings_match);
                    copy_location_info(identifier, bin);
                    identifier->enclosing_scope = compiler->global_scope;
                    
                    call->function_or_function_ptr = identifier;
                    call->argument_list.add(bin->left);
                    call->argument_list.add(bin->right);
                    
                    typecheck_expression(call);
                    bin->substitution = call;

                    if (bin->operator_type == Token::NE_OP) {
                        auto _not = make_binary(compiler, Token::NE_OP, call, make_bool_literal(compiler, true, /*location=*/bin), /*location=*/bin);

                        // @Speed we can probably just assign _not->type_info to compiler->type_bool.
                        typecheck_expression(_not);

                        bin->substitution = _not;
                    }
                    return;
                }
            }
            
            return;
        }
        
        case AST_UNARY_EXPRESSION: {
            auto un = static_cast<Ast_Unary_Expression *>(expression);
            
            typecheck_expression(un->expression);
            if (compiler->errors_reported) return;
            
            if (un->operator_type == Token::STAR) {
                if (!expression_is_lvalue(un->expression, true)) {
                    compiler->report_error(un, "lvalue required as unary '%c' operand.\n", un->operator_type);
                }
                un->type_info = compiler->make_pointer_type(get_type_info(un->expression));
                
                auto expr = un->expression;
                while (expr->substitution) expr = expr->substitution;
                
                if (expr->type == AST_UNARY_EXPRESSION) {
                    auto second = static_cast<Ast_Unary_Expression *>(expr);
                    if (second->operator_type == Token::DEREFERENCE_OR_SHIFT && expression_is_lvalue(second->expression, false)) {
                        // remove this sequence of *<< because it is ineffective.
                        un->substitution = second->expression;
                        return;
                    }
                }
            } else if (un->operator_type == Token::DEREFERENCE_OR_SHIFT) {
                auto type = get_type_info(un->expression);
                if (type->type != Ast_Type_Info::POINTER) {
                    compiler->report_error(un, "Cannot use '<<' on a non-pointer expression.\n");
                    return;
                }
                
                un->type_info = type->pointer_to;
            } else if (un->operator_type == Token::MINUS) {
                auto type = get_type_info(un->expression);
                if (type->type != Ast_Type_Info::INTEGER && type->type != Ast_Type_Info::FLOAT) {
                    compiler->report_error(un, "Unary '-' is only valid for integer for float operands.\n");
                    return;
                }
                
                auto lit = resolves_to_literal_value(un->expression);
                // @TODO this isnt exactly correct...
                /*
                if (lit) {
                if (type->type == Ast_Type_Info::INTEGER) {
                lit->integer_value = (-lit->integer_value);
                } else if (type->type == Ast_Type_Info::FLOAT) {
                lit->float_value = (-lit->float_value);
                } else assert(0);
                
                un->substitution = lit;
                }
                */
                
                // @Incomplete I think, should we warn about unary minus on unsiged integers?
                un->type_info = type;
            }
            
            assert(un->type_info);
            return;
        }
        
        case AST_LITERAL: {
            auto lit = static_cast<Ast_Literal *>(expression);
            
            // @Incomplete
            
            // @Incomplete if we have a float literal but want an int type, keep a float type and let the implicit cast system do its job
            if (lit->literal_type == Ast_Literal::INTEGER) {
                if (want_numeric_type && (want_numeric_type->type == Ast_Type_Info::INTEGER || want_numeric_type->type == Ast_Type_Info::FLOAT)) {
                    // @Incomplete check that number can fit in target type
                    // @Incomplete cast to float if we have an int literal
                    lit->type_info = want_numeric_type;
                    
                    
                    // @Cleanup I'm mutating the literal for now, but this would be a good place to use substitution, I think
                    // Or since literal ints are considered completely typeless up until this point, maybe this is the right thing to do
                    if (want_numeric_type->type == Ast_Type_Info::FLOAT) {
                        lit->literal_type = Ast_Literal::FLOAT;
                        // @Cleanup the u64 cast
                        lit->float_value = static_cast<double>((u64)lit->integer_value);
                    }
                } else {
                    lit->type_info = compiler->type_int32;
                }
            }
            
            if (lit->literal_type == Ast_Literal::FLOAT) {
                if (want_numeric_type && want_numeric_type->type == Ast_Type_Info::FLOAT) lit->type_info = want_numeric_type;
                else lit->type_info = compiler->type_float64; // @TODO we should probably have a check that verifies if the literal can fit in a 32-bit float and then default to that.
            }
            
            if (lit->literal_type == Ast_Literal::STRING)  lit->type_info = compiler->type_string;
            
            if (lit->literal_type == Ast_Literal::BOOL) lit->type_info = compiler->type_bool;
            
            if (lit->literal_type == Ast_Literal::NULLPTR) {
                lit->type_info = compiler->type_ptr_void;
                if (want_numeric_type && want_numeric_type->type == Ast_Type_Info::POINTER) {
                    lit->type_info = want_numeric_type;
                }
            }
            
            assert(lit->type_info);
            
            return;
        }
        
        case AST_FUNCTION: {
            auto function = static_cast<Ast_Function *>(expression);
            typecheck_function(function);
            return;
        }
        
        case AST_FUNCTION_CALL: {
            auto call = static_cast<Ast_Function_Call *>(expression);
            
            auto subexpression = call->function_or_function_ptr;
            
            // Check for the os() case early so we don't error on the identifier lookup.
            if (subexpression->type == AST_IDENTIFIER) {
                auto identifier = static_cast<Ast_Identifier *>(subexpression);
                
                if (identifier->name == compiler->atom_os) {
                    if (call->argument_list.count != 1) {
                        compiler->report_error(call, "os() operator only accepts one argument.\n");
                        return;
                    }
                    
                    Ast_Os *os = SEMA_NEW(Ast_Os);
                    copy_location_info(os, call);
                    os->expression = call->argument_list[0];
                    typecheck_expression(os);
                    call->substitution = os;
                    return;
                }
            }
            
            typecheck_expression(subexpression, want_numeric_type, true);
            if (compiler->errors_reported) return;

            Ast_Expression *implicit_argument = nullptr;
            if (subexpression->type == AST_DEREFERENCE) {
                auto deref = static_cast<Ast_Dereference *>(subexpression);
                if (!deref->is_type_dereference) {
                    auto left_type = get_type_info(deref->left);
                    if ((is_pointer_type(left_type) && is_struct_type(left_type->pointer_to))
                        || is_struct_type(left_type)) {
                        implicit_argument = deref->left;

                        if (is_struct_type(left_type)) {
                            // Turn this struct into a pointer
                            auto unary = make_unary(compiler, Token::STAR, implicit_argument);
                            // @Speed we can probably just assign the right type here.
                            typecheck_expression(unary);

                            implicit_argument = unary;
                        }
                    }
                }
            }

            if (implicit_argument) {
                call->argument_list.insert(0, implicit_argument);
                call->implicit_argument_inserted = true;
            }

            while (subexpression->substitution) subexpression = subexpression->substitution;
            
            if (subexpression->type == AST_IDENTIFIER) {
                auto identifier = static_cast<Ast_Identifier *>(subexpression);
                
                if (identifier->overload_set.count) {
                    auto overload_set = identifier->overload_set;
                    
                    if (overload_set.count == 0) {
                        compiler->report_error(call, "Function call identifier does not name a function.");
                        return;
                    }
                    
                    // assert(identifier->type_info == compiler->type_void);
                    
                    Ast_Function *function = nullptr;
                    if (overload_set.count == 1) {
                        function = overload_set[0];
                        typecheck_function_header(function);
                        if (compiler->errors_reported) return;
                        
                        if (function->is_template_function) {
                            function = get_polymorph_for_function_call(function, call);
                            if (compiler->errors_reported) return;
                        }
                    } else {
                        const u64 U64_MAX = 0xFFFFFFFFFFFFFFFF;
                        u64 lowest_score = U64_MAX;
                        for (auto overload : overload_set) {
                            if (overload->is_template_function) {
                                overload = get_polymorph_for_function_call(overload, call);
                                if (compiler->errors_reported) return;
                                
                                if (!overload) continue; // no polymorphs that match this call, so skip it
                            }
                            
                            typecheck_function_header(overload);
                            if (compiler->errors_reported) return;
                            
                            auto tuple = function_call_is_viable(call, get_type_info(overload), false);
                            if (compiler->errors_reported) return;
                            
                            bool viable = tuple.item1;
                            u64  score  = tuple.item2;
                            
                            if (viable) {
                                if (score < lowest_score) {
                                    lowest_score = score;
                                    function = overload;
                                }
                            }
                        }
                    }
                    
                    if (!function) {
                        // @Incomplete print visible overload candidates
                        compiler->report_error(call, "No viable overload for function call.\n");
                        return;
                    }
                    
                    function_call_is_viable(call, get_type_info(function), true);
                    if (compiler->errors_reported) return;
                    
                    if (function->return_decl) {
                        call->type_info = function->return_decl->type_info;
                    } else {
                        call->type_info = compiler->type_void;
                    }
                    
                    identifier->resolved_declaration = function;
                    identifier->type_info = function->type_info;
                    return;
                }
            }
            
            // fall through case for expressions that generate function pointers
            auto info = get_type_info(subexpression);
            auto tuple = function_call_is_viable(call, info, true);
            
            bool viable = tuple.item1;
            if (viable) {
                call->type_info = info->return_type;
            } else {
                assert(compiler->errors_reported);
            }
            return;
        }
        
        case AST_DEREFERENCE: {
            auto deref = static_cast<Ast_Dereference *>(expression);
            typecheck_expression(deref->left);
            if (compiler->errors_reported) return;
            
            if (get_type_info(deref->left)->type == Ast_Type_Info::POINTER) {
                // we allow you to dereference once through a pointer
                // here we insert some desugaring that expands pointer.field into (<<pointer).field
                
                auto un = make_unary(compiler, Token::DEREFERENCE_OR_SHIFT, deref->left);
                copy_location_info(un, deref);

                typecheck_expression(un);
                deref->left = un; // Dont set substitution here, otherwise we'll infinite loop
            }
            
            auto left_type = get_type_info(deref->left);
            assert(left_type);
            
            bool is_type_use = false;
            auto left = deref->left;
            if (left_type->type == Ast_Type_Info::TYPE) {
                is_type_use = true;
                while (left->substitution) left = left->substitution;
                
                if (left->type == AST_IDENTIFIER) {
                    auto identifier = static_cast<Ast_Identifier *>(left);
                    
                    left = identifier->resolved_declaration;
                    left_type = get_type_info(left);
                }
                
                if (left->type == AST_TYPE_ALIAS) {
                    auto alias = static_cast<Ast_Type_Alias *>(left);
                    
                    left_type = alias->type_value->alias_of;
                    while (left_type->type == Ast_Type_Info::ALIAS) left_type = left_type->alias_of;
                    
                    assert(left_type);
                    assert(alias->type_value->type == Ast_Type_Info::ALIAS);
                } else if (left->type == AST_STRUCT) {
                    auto _struct = static_cast<Ast_Struct *>(left);
                    
                    left_type = _struct->type_value;
                    assert(left_type);
                }
            }

            deref->is_type_dereference = is_type_use;
            
            if (left_type->type != Ast_Type_Info::STRING &&
                left_type->type != Ast_Type_Info::ARRAY  &&
                left_type->type != Ast_Type_Info::STRUCT) {
                compiler->report_error(deref, "Attempt to dereference a type that is not a string, struct, or array!\n");
                return;
            }
            
            // @Hack until we have field members in the type_info (data and length would be field members of string)
            assert(deref->field_selector && deref->field_selector->name);
            Atom *field_atom = deref->field_selector->name;
            
            if (left_type->type == Ast_Type_Info::STRING) {
                if (field_atom == compiler->atom_data) {
                    deref->element_path_index = 0;
                    deref->byte_offset = 0;
                    
                    // @Hack @Incomplete
                    deref->type_info = compiler->type_string_data;
                } else if (field_atom == compiler->atom_length) {
                    deref->element_path_index = 1;
                    // @Hack @Cleanup
                    // @Hack @Cleanup
                    // @Hack @Cleanup
                    deref->byte_offset = 8; // @TargetInfo
                    deref->type_info = compiler->type_string_length;
                } else {
                    String field_name = field_atom->name;
                    compiler->report_error(deref, "No member '%.*s' in type string.\n", field_name.length, field_name.data);
                }
            } else if (left_type->type == Ast_Type_Info::ARRAY) {
                if (left_type->array_element_count == -1) {
                    if (field_atom == compiler->atom_data) {
                        deref->element_path_index = 0;
                        deref->byte_offset = 0;
                        
                        // @Hack @Incomplete
                        deref->type_info = compiler->make_pointer_type(left_type->array_element);
                    } else if (field_atom == compiler->atom_count) {
                        deref->element_path_index = 1;
                        // @Hack @Cleanup
                        // @Hack @Cleanup
                        // @Hack @Cleanup
                        deref->byte_offset = 8; // @TargetInfo
                        deref->type_info = compiler->type_array_count;
                    } else if (left_type->is_dynamic && field_atom == compiler->atom_allocated) {
                        deref->element_path_index = 2;
                        // @Hack @Cleanup
                        // @Hack @Cleanup
                        // @Hack @Cleanup
                        deref->byte_offset = 16; // @TargetInfo
                        deref->type_info = compiler->type_array_count;
                    } else {
                        String field_name = field_atom->name;
                        compiler->report_error(deref, "No member '%.*s' in type array.\n", field_name.length, field_name.data);
                    }
                } else {
                    assert(left_type->is_dynamic == false);
                    
                    if (field_atom == compiler->atom_data) {
                        auto index_lit = make_integer_literal(compiler, 0, compiler->type_array_count);
                        copy_location_info(index_lit, deref);
                        
                        auto index = make_array_index(compiler, deref->left, index_lit);
                        copy_location_info(index, deref);
                        
                        auto addr = make_unary(compiler, Token::STAR, index);
                        copy_location_info(addr, deref);
                        
                        typecheck_expression(addr);
                        deref->substitution = addr;
                    } else if (field_atom == compiler->atom_count) {
                        auto lit = make_integer_literal(compiler, left_type->array_element_count, compiler->type_array_count);
                        copy_location_info(lit, deref);
                        deref->substitution = lit;
                    } else {
                        String field_name = field_atom->name;
                        compiler->report_error(deref, "No member '%.*s' in known-size array.\n", field_name.length, field_name.data);
                    }
                }
            } else if (left_type->type == Ast_Type_Info::STRUCT) {
                // @Incomplete this should perform a scope lookup for a declaration so we can handle
                // lets, functions, typealiases, etc..
                bool found = false;
                for (auto member : left_type->struct_members) {
                    if (member.is_let) continue;
                    
                    if (member.name == field_atom) {
                        found = true;
                        
                        deref->element_path_index = member.element_index;
                        deref->type_info = member.type_info;
                        deref->byte_offset = -1; // @Incomplete
                        break;
                    }
                }
                
                if (found && is_type_use) {
                    compiler->report_error(deref, "Attempt to use struct variable member without an instance!\n");
                    return;
                }
                
                if (!found) {
                    auto _struct = left_type->struct_decl;
                    assert(_struct);
                    
                    auto decl = find_declaration_for_atom_in_scope(&_struct->member_scope, field_atom);
                    if (decl && decl->type == AST_DECLARATION) {
                        auto declaration = static_cast<Ast_Declaration *>(decl);
                        
                        // this is not supposed to happen because regular var's should be handled by the above code.
                        if (is_type_use) assert(declaration->is_let);
                    } else if (decl && decl->type == AST_FUNCTION) {
                        // @Hack substitute to an identifier to get the quick benefit of the overloading sytem.

                        auto ident = make_identifier(compiler, field_atom);
                        ident->enclosing_scope = &_struct->member_scope;

                        // @Cutnpaste from the identifier stuff
                        // @Cleanup the only difference in this version is that we're only
                        // checking the struct scope instead of looking up through all parent scopes.
                        assert(ident->overload_set.count == 0);
                        collect_function_overloads_for_atom_in_scope(ident->name, ident->enclosing_scope, &ident->overload_set);
                        
                        if (!overload_set_allowed && ident->overload_set.count > 1) {
                            String name = ident->name->name;
                            compiler->report_error(ident, "Ambiguous use of overloaded function '%.*s' (%d overloads).\n", name.length, name.data, ident->overload_set.count);
                            
                            
                            for (auto overload: ident->overload_set) {
                                compiler->report_error(overload, "DEBUG: here\n");
                            }
                            
                            return;
                        } else if (!overload_set_allowed) {
                            assert(ident->overload_set.count == 1);
                            
                            typecheck_expression(decl);
                            ident->resolved_declaration = decl;
                            ident->type_info = get_type_info(decl);
                            return;
                        }

                        // resolved_declaration and type_info will be resolved by the Ast_Function_Call code.
                        // Set to void for now, Ast_Function_Call code will either error or fix this up.
                        ident->type_info = compiler->type_void;

                        decl = ident;
                    }
                    
                    if (decl) {
                        typecheck_expression(decl);
                        if (compiler->errors_reported) return;
                        
                        deref->substitution = decl;
                        found = true;
                    }
                }
                
                if (!found) {
                    String field_name = field_atom->name;
                    String name = left_type->struct_decl->identifier->name->name;
                    compiler->report_error(deref, "No member '%.*s' in struct %.*s.\n", field_name.length, field_name.data, name.length, name.data);
                }
            }
            
            return;
        }
        
        case AST_IF: {
            auto _if = static_cast<Ast_If *>(expression);
            
            auto result = typecheck_and_implicit_cast_single_expression(_if->condition, compiler->type_bool, ALLOW_COERCE_TO_BOOL);
            _if->condition = result.item2;
            
            if (compiler->errors_reported) return;

            auto cond = _if->condition;
            if (get_type_info(cond)->type != Ast_Type_Info::BOOL) {
                // @TODO check for coercion to bool
                compiler->report_error(cond, "'if' condition isn't of boolean type.\n");
            }
            
            if (_if->then_statement) typecheck_expression(_if->then_statement);
            if (_if->else_statement) typecheck_expression(_if->else_statement);
            
            return;
        }
        
        case AST_WHILE: {
            auto loop = static_cast<Ast_While *>(expression);
            
            typecheck_expression(loop->condition);
            
            if (compiler->errors_reported) return;
            
            auto cond = loop->condition;
            if (get_type_info(cond)->type != Ast_Type_Info::BOOL) {
                // @TODO check for coercion to bool
                compiler->report_error(cond, "'while' condition isn't of boolean type.\n");
            }
            
            if (loop->statement) typecheck_expression(loop->statement);
            
            return;
        }
        
        case AST_FOR: {
            auto _for = static_cast<Ast_For *>(expression);
            if (!_for->initial_iterator_expression) {
                compiler->report_error(_for, "'for' must be followed by an expression.\n");
                return;
            }
            
            typecheck_expression(_for->initial_iterator_expression);
            
            if (compiler->errors_reported) return;
            
            if (!_for->upper_range_expression) {
                auto init_type = get_type_info(_for->initial_iterator_expression);
                if (init_type->type == Ast_Type_Info::INTEGER) {
                    compiler->report_error(_for, "'for' must specify an upper-range. Ex: for 0..1\n");
                    return;
                }
            } else {
                auto init_type = get_type_info(_for->initial_iterator_expression);
                if (init_type->type != Ast_Type_Info::INTEGER) {
                    compiler->report_error(_for, "'..' operator may only be preceeded by an integer expression.\n");
                    return;
                }
                
                typecheck_expression(_for->upper_range_expression);
                if (compiler->errors_reported) return;
                
                auto init_expr = _for->initial_iterator_expression;
                auto upper_expr = _for->upper_range_expression;
                bool allow_coerce_to_ptr_void = false;
                typecheck_and_implicit_cast_expression_pair(init_expr, upper_expr, &_for->initial_iterator_expression, &_for->upper_range_expression, allow_coerce_to_ptr_void);
                
                init_type = get_type_info(_for->initial_iterator_expression);
                auto upper_type = get_type_info(_for->upper_range_expression);
                if (upper_type->type != Ast_Type_Info::INTEGER) {
                    compiler->report_error(_for->upper_range_expression, "'for' upper-range must be an integer expression.\n");
                    return;
                }
                
                if (!types_match(init_type, upper_type)) {
                    compiler->report_error(_for, "'for' lower-range and upper-range types do not match!\n");
                }
            }
            
            auto init_type = get_type_info(_for->initial_iterator_expression);
            if (!is_int_type(init_type)) {
                // @Incomplete
                bool supports_iteration_interface = type_is_iterable(init_type);
                
                if (!supports_iteration_interface) {
                    compiler->report_error(_for->initial_iterator_expression, "Type of expression in 'for' condition is not iterable. Must be an integer range or a type that supports the iteration-inteface (.count, []).");
                    return;
                }
            }
            
            if (!is_int_type(init_type)) {
                auto count_expr = make_dereference(compiler, _for->initial_iterator_expression, compiler->atom_count);
                typecheck_expression(count_expr);
                
                auto zero = make_integer_literal(compiler, 0, get_type_info(count_expr));
                auto it_index_ident = make_identifier(compiler, compiler->atom_it_index);
                copy_location_info(it_index_ident, _for);
                it_index_ident->enclosing_scope = &_for->body;
                {
                    Ast_Declaration *decl = SEMA_NEW(Ast_Declaration);
                    copy_location_info(decl, _for);
                    decl->identifier = it_index_ident;
                    copy_location_info(decl->identifier, _for);
                    decl->initializer_expression = zero;
                    decl->is_let = true;
                    decl->is_readonly_variable = true;
                    
                    _for->iterator_index_decl = decl;
                    
                    typecheck_expression(_for->iterator_index_decl);
                    if (compiler->errors_reported) return;
                }
                
                {
                    Ast_Expression *indexed = make_array_index(compiler, _for->initial_iterator_expression, it_index_ident);
                    
                    if (_for->is_element_pointer_iteration) {
                        indexed = make_unary(compiler, Token::STAR, indexed);
                    }

                    Ast_Declaration *decl = SEMA_NEW(Ast_Declaration);
                    copy_location_info(decl, _for);
                    decl->identifier = make_identifier(compiler, compiler->atom_it);
                    copy_location_info(decl->identifier, decl);
                    decl->initializer_expression = indexed;
                    decl->is_let = true;
                    decl->is_readonly_variable = true;
                    // decl->type_info = get_type_info(indexed);
                    
                    _for->iterator_decl = decl;
                }
                
                assert(_for->upper_range_expression == nullptr);
                _for->upper_range_expression = count_expr;
            }
            
            if (!_for->iterator_decl) {
                // for integer ranges only
                Ast_Declaration *decl = SEMA_NEW(Ast_Declaration);
                copy_location_info(decl, _for);
                decl->identifier = make_identifier(compiler, compiler->atom_it);
                copy_location_info(decl->identifier, _for);
                decl->identifier->enclosing_scope = &_for->body;
                decl->initializer_expression = _for->initial_iterator_expression;
                decl->is_let = true;
                decl->is_readonly_variable = true;
                
                _for->iterator_decl = decl;
            }
            
            // create a temporary scope for the iterator variable
            Ast_Scope *scope = &_for->iterator_declaration_scope;
            assert(_for->iterator_decl);
            scope->declarations.add(_for->iterator_decl);
            if (_for->iterator_index_decl) scope->declarations.add(_for->iterator_index_decl);
            
            typecheck_expression(_for->iterator_decl);
            if (compiler->errors_reported) return;
            
            assert(get_type_info(_for->iterator_decl));
            
            typecheck_expression(&_for->body);
            
            if (compiler->errors_reported) return;
            
            _for->type_info = compiler->type_void;
            return;
        }
        
        case AST_SCOPE: {
            auto scope = static_cast<Ast_Scope *>(expression);
            typecheck_scope(scope);
            scope->type_info = compiler->type_void;
            return;
        }
        
        case AST_RETURN: {
            auto ret = static_cast<Ast_Return *>(expression);
            
            auto function = ret->owning_function;
            if (function->return_decl) {
                // since we are currently in the scope of this function, return_decl should be gauranteed to be typechecked already.
                assert(get_type_info(function->return_decl));
                
                auto return_type = get_type_info(function->return_decl);
                
                if (!ret->expression) {
                    String name = type_to_string(return_type);
                    compiler->report_error(ret, "'return' statement must return an expression of function return type %.*s.\n", name.length, name.data);
                    return;
                }
                
                typecheck_expression(ret->expression);
                bool allow_coerce_to_ptr_void = false;
                typecheck_and_implicit_cast_expression_pair(ret->expression, function->return_decl, &ret->expression, nullptr, allow_coerce_to_ptr_void);
                if (compiler->errors_reported) return;
                
                auto value_type = get_type_info(ret->expression);
                
                if (!types_match(value_type, return_type)) {
                    auto wanted = type_to_string(return_type);
                    auto given  = type_to_string(value_type);
                    compiler->report_error(ret->expression, "Type of return expression does not match function return type. (Wanted %.*s, Given %.*s).\n",
                                           wanted.length, wanted.data, given.length, given.data);
                    free(wanted.data);
                    free(given.data);
                    return;
                }
            } else if (ret->expression) {
                compiler->report_error(ret, "Cannot return non-void expression in function returning void.\n");
                
                typecheck_expression(ret->expression);
            }
            
            return;
        }
        
        case AST_CAST: {
            auto cast = static_cast<Ast_Cast *>(expression);
            
            typecheck_expression(cast->expression);
            
            if (compiler->errors_reported) return;
            
            auto expr_type = get_type_info(cast->expression);
            
            Ast_Type_Info *target = nullptr;
            if (cast->target_type_inst) {
                target = resolve_type_inst(cast->target_type_inst);
            } else {
                if (!want_numeric_type) {
                    compiler->report_error(cast, "Cannot infer a type for cast().\n");
                    return;
                }
                
                target = want_numeric_type;
            }
            
            assert(target);
            
            cast->type_info = target;
            
            if (!is_valid_primitive_cast(target, expr_type)) {
                // @TODO print the types we're trying to cast between
                compiler->report_error(cast, "Cast is invalid.\n");
            }
            
            return;
        }
        
        case AST_TYPE_ALIAS: {
            auto alias = static_cast<Ast_Type_Alias *>(expression);
            if (alias->internal_type_inst) {
                resolve_type_inst(alias->internal_type_inst);
                if (compiler->errors_reported) return;
                
                assert(alias->internal_type_inst->type_value);
                alias->type_value = alias->internal_type_inst->type_value;
            } else {
                assert(alias->type_value);
            }
            alias->type_info = compiler->type_info_type;
            return;
        }
        
        case AST_STRUCT: {
            auto _struct = static_cast<Ast_Struct *>(expression);
            
            // Set this early so we dont recurse indefinitely
            _struct->type_value = make_struct_type(_struct);
            _struct->type_info = compiler->type_info_type;
            
            // flag stuct member declarations
            for (auto _decl : _struct->member_scope.declarations) {
                if (_decl->type == AST_DECLARATION) {
                    auto decl = static_cast<Ast_Declaration *>(_decl);
                    decl->is_struct_member = true;
                }
            }
            
            {
                auto info = _struct->type_value;
                assert(info->type == Ast_Type_Info::STRUCT);
                assert(info->struct_decl == _struct);
                
                s64 size_cursor = 0;
                s64 biggest_alignment = 1;
                s64 element_path_index = 0;
                
                // This is likely super @Incomplete
                for (auto expr : _struct->member_scope.declarations) {
                    if (expr->type == AST_DECLARATION) {
                        
                        // @Cleanup @Hack we need to be able to handle other structs, functions, typealiases or at least punt on them.
                        auto decl = static_cast<Ast_Declaration *>(expr);
                        typecheck_expression(decl);
                        if (compiler->errors_reported) return;
                        
                        assert(decl && decl->type_info);
                        
                        Ast_Type_Info::Struct_Member member;
                        member.name = decl->identifier->name;
                        member.type_info = decl->type_info;
                        member.is_let = decl->is_let;
                        
                        if (!member.is_let) {
                            member.element_index = element_path_index;
                            element_path_index++;
                        }
                        
                        size_cursor = pad_to_alignment(size_cursor, member.type_info->alignment);
                        member.offset_in_struct = size_cursor;
                        size_cursor += member.type_info->size;
                        
                        if (member.type_info->alignment > biggest_alignment) {
                            biggest_alignment = member.type_info->alignment;
                        }

                        info->struct_members.add(member);
                    }
                }
                
                info->alignment = biggest_alignment;
                info->size = size_cursor;
                info->stride = pad_to_alignment(info->size, info->alignment);

                compiler->add_to_type_table(info);
            }
            
            typecheck_scope(&_struct->member_scope);
            if (compiler->errors_reported) return;
            
            return;
        }
        
        case AST_ARRAY_DEREFERENCE: {
            auto deref = static_cast<Ast_Array_Dereference *>(expression);
            
            assert(deref->array_or_pointer_expression);
            
            if (!deref->index_expression) {
                compiler->report_error(deref, "Array index expression missing subscript.\n");
                return;
            }
            
            typecheck_expression(deref->array_or_pointer_expression);
            if (compiler->errors_reported) return;
            
            typecheck_expression(deref->index_expression);
            if (compiler->errors_reported) return;
            
            auto array_type = get_type_info(deref->array_or_pointer_expression);
            if (array_type->type != Ast_Type_Info::ARRAY   && array_type->type != Ast_Type_Info::POINTER &&
                array_type->type != Ast_Type_Info::STRING) {
                compiler->report_error(deref->array_or_pointer_expression, "Expected array, string, or pointer for index expression, but got something else.\n");
                return;
            }
            
            auto index_type = get_type_info(deref->index_expression);
            if (index_type->type != Ast_Type_Info::INTEGER) {
                compiler->report_error(deref->index_expression, "Array index subscript must be of integer type.\n");
                return;
            }
            
            if (array_type->type == Ast_Type_Info::ARRAY &&
                array_type->array_element_count >= 0) {
                auto lit = resolves_to_literal_value(deref->index_expression);
                if (lit) {
                    auto lit_type = get_type_info(lit);
                    auto value = lit->integer_value;
                    
                    bool out_of_range = false;
                    if (lit_type->is_signed) {
                        if (value < 0) out_of_range = true;
                    }
                    
                    if (value >= array_type->array_element_count) out_of_range = true;
                    
                    if (out_of_range) {
                        compiler->report_error(deref->index_expression, "Index value %lld is outside the range of known-size array (size: %lld).\n", value, array_type->array_element_count);
                    }
                }
            }
            
            if (array_type->type == Ast_Type_Info::ARRAY) deref->type_info = array_type->array_element;
            else if (array_type->type == Ast_Type_Info::POINTER) deref->type_info = array_type->pointer_to;
            else if (array_type->type == Ast_Type_Info::STRING) deref->type_info = compiler->type_uint8;
            else assert(false);
            
            return;
        }
        
        case AST_SIZEOF: {
            Ast_Sizeof *size = static_cast<Ast_Sizeof *>(expression);
            
            if (!size->target_type_inst) {
                compiler->report_error(size, "sizeof() must specify a type to take the size of.\n");
                return;
            }
            
            auto type = resolve_type_inst(size->target_type_inst);
            assert(type->size >= 0);
            
            auto lit = make_integer_literal(compiler, type->size, compiler->type_int32);
            copy_location_info(lit, size);

            size->type_info = lit->type_info;
            size->substitution = lit;
            return;
        }
        case AST_OS: {
            auto os = static_cast<Ast_Os *>(expression);
            
            // Don't typecheck expression here as we only care about
            // checking against the parsed Atom. Though, I'm not sure
            // if this is good behavior in the long run. -josh 23 June 2019
            assert(os->expression);
            
            auto expr = os->expression;
            while (expr->substitution) expr = expr->substitution;
            
            if (expr->type != AST_IDENTIFIER) {
                compiler->report_error(os->expression, "Argument to os() operator must be an identifier.\n");
                return;
            }
            
            auto ident = static_cast<Ast_Identifier *>(expr);
            Ast_Literal *lit = SEMA_NEW(Ast_Literal);
            copy_location_info(lit, os);
            lit->literal_type = Ast_Literal::BOOL;
            lit->type_info = compiler->type_bool;
            lit->bool_value = false;
            
            // @TODO @FixMe this should be based off of what the LLVM target is
#ifdef WIN32
            lit->bool_value = (ident->name == compiler->atom_Windows);
#elif defined(MACOSX)
            lit->bool_value = (ident->name == compiler->atom_MacOSX);
#elif defined(LINUX)
            lit->bool_value = (ident->name == compiler->atom_Linux);
#else
            assert(false);
#endif
            Atom *name = ident->name;
            bool valid_option = (name == compiler->atom_Windows) || (name == compiler->atom_MacOSX) || (name == compiler->atom_Linux);
            
            if (!valid_option) {
                String op = name->name;
                compiler->report_error(ident, "Unrecognized os() option '%.*s'.\n", op.length, op.data);
                return;
            }
            
            String op = name->name;
            // printf("os(): %.*s: %s\n", op.length, op.data, lit->bool_value ? "true" : "false");
            
            os->type_info = lit->type_info;
            os->substitution = lit;
            return;
        }
        
        case AST_LIBRARY: {
            auto lib = static_cast<Ast_Library *>(expression);
            
            // @TODO @FixMe this should be based off of what the LLVM target is
#ifndef MACOSX
            if (lib->is_framework) {
                compiler->report_error(lib, "'framework' is only valid for macOS, iPadOS, iOS, tvOS, and watchOS targets.\n");
                return;
            }
#endif
            
            compiler->libraries.add(lib);
            return;
        }

        case AST_CONTROL_FLOW: {
            auto flow = static_cast<Ast_Control_Flow *>(expression);

            Ast_Expression *target_statement = nullptr;
            auto scope = flow->current_scope;
            while (scope) {
                if (scope->owning_statement) {
                    auto stmt = scope->owning_statement;

                    if (stmt->type == AST_FOR || stmt->type == AST_WHILE) {
                        target_statement = stmt;
                        break;
                    }
                } else if (scope->owning_function) {
                    // We hit the top-level scope of the function we're in, so break, otherwise we'll mistakenly be valid for loops of outer functions.
                    break;
                }

                scope = scope->parent;
            }

            if (!target_statement) {
                String name;
                if      (flow->control_type == Token::KEYWORD_BREAK)    name = to_string("break");
                else if (flow->control_type == Token::KEYWORD_CONTINUE) name = to_string("continue");
                else assert(false);

                compiler->report_error(flow, "%.*s statement not made within a loop.\n", name.length, name.data);
                return;
            }

            flow->target_statement = target_statement;
            return;
        }
    }
    
    assert(false);
    return;
}

Ast_Type_Info *Sema::resolve_type_inst(Ast_Type_Instantiation *type_inst) {
    if (type_inst->type_value) return type_inst->type_value;
    
    if (type_inst->pointer_to) {
        auto pointee = resolve_type_inst(type_inst->pointer_to);
        type_inst->type_value = compiler->make_pointer_type(pointee);
        return type_inst->type_value;
    }
    
    if (type_inst->builtin_primitive) {
        type_inst->type_value = type_inst->builtin_primitive;
        return type_inst->type_value;
    }
    
    if (type_inst->typename_identifier) {
        typecheck_expression(type_inst->typename_identifier);
        
        if (compiler->errors_reported) return nullptr;
        
        auto ident = type_inst->typename_identifier;
        
        if (ident->type_info->type != Ast_Type_Info::TYPE) {
            String name = ident->name->name;
            compiler->report_error(ident, "Identifier '%.*s' doesn't name a type.\n", name.length, name.data);
            return nullptr;
        }
        
        auto decl = ident->resolved_declaration;
        
        if (decl->type == AST_TYPE_ALIAS) {
            auto alias = static_cast<Ast_Type_Alias *>(decl);
            typecheck_expression(alias);
            if (compiler->errors_reported) return nullptr;
            
            assert(alias->type_value);
            type_inst->type_value = alias->type_value;
            return type_inst->type_value;
        } else if (decl->type == AST_STRUCT) {
            auto _struct = static_cast<Ast_Struct *>(decl);
            typecheck_expression(_struct);
            type_inst->type_value = _struct->type_value;
            return type_inst->type_value;
        } else {
            assert(false);
        }
    }
    
    if (type_inst->array_element_type) {
        auto element = resolve_type_inst(type_inst->array_element_type);
        if (compiler->errors_reported) return nullptr;
        
        if (!element) {
            compiler->report_error(type_inst, "Internal error: Array-type must specify an element type.\n");
            return nullptr;
        }
        
        if (type_inst->array_size_expression) {
            auto size_expr = type_inst->array_size_expression;
            
            typecheck_expression(size_expr);
            if (compiler->errors_reported) return nullptr;
            
            if (get_type_info(size_expr)->type != Ast_Type_Info::INTEGER) {
                compiler->report_error(size_expr, "Array-type size specifier must be an integer.\n");
                return nullptr;
            }
            
            auto lit = folds_to_literal(size_expr);
            
            if (!lit) {
                compiler->report_error(type_inst, "Array-type size specifier must resolve to a literal expression.\n");
                return nullptr;
            }
            
            auto array_type = compiler->make_array_type(element, lit->integer_value, false);
            type_inst->type_value = array_type;
            return type_inst->type_value;
        }
        
        auto array_type = compiler->make_array_type(element, -1, type_inst->array_is_dynamic);
        type_inst->type_value = array_type;
        return type_inst->type_value;
    }
    
    if (type_inst->function_header) {
        typecheck_function_header(type_inst->function_header, /*is_for_type_instantiation*/true);
        
        if (compiler->errors_reported) return nullptr;
        
        type_inst->type_value = get_type_info(type_inst->function_header);
        assert(type_inst->type_value);
        return type_inst->type_value;
    }
    
    assert(false);
    return nullptr;
}

void Sema::typecheck_function_header(Ast_Function *function, bool is_for_type_instantiation) {
    if (function->type_info) return;
    
    if (compiler->errors_reported) return;
    
    if (function->is_c_function && function->is_template_function) {
        compiler->report_error(function, "Function declared @c_function cannot have template arguments.\n");
    }
    
    if (function->is_template_function) {
        if (function->is_marked_metaprogram) {
            compiler->report_error(function, "@metaprogram is only valid on the main entry point function.\n");
        }
        // we dont typecheck template functions, we only typecheck polymorphs, which will make it here on their own.
        function->type_info = compiler->type_void;
        return;
    }
    
    if (function->polymorphic_type_alias_scope) {
        for (auto a: function->polymorphic_type_alias_scope->declarations) {
            typecheck_expression(a);
        }
    }
    
    for (auto &a : function->arguments) {
        a->is_readonly_variable = true;
        typecheck_expression(a);
    }
    
    if (function->return_decl) typecheck_expression(function->return_decl);
    
    if (compiler->errors_reported) return;
    
    if (function->is_c_varargs && !function->is_c_function) {
        compiler->report_error(function, "Function must be tagged @c_function in order to use temporary_c_vararg.\n");
        return;
    }
    
    if (function->is_c_function) {
        // @TODO error if a C function is declared returning a tuple
        /*
        if (function->returns.count > 1) {
        compiler->report_error(function, "Function tagged @c_function may only have 1 return value.\n");
        }
        */
    }
    
    function->type_info = compiler->make_function_type(function);
    assert(function->type_info->type == Ast_Type_Info::FUNCTION);

    if (!is_for_type_instantiation && function->linkage_name == to_string("")) {
        if (function->is_c_function) {
            function->linkage_name = function->identifier->name->name;
        } else {
            function->linkage_name = get_mangled_name(compiler, function);
            String name = function->linkage_name;
            // printf("Mangled name: '%.*s'\n", name.length, name.data);
        }
    }
}

void Sema::typecheck_function(Ast_Function *function) {
    typecheck_function_header(function);
    
    if (function->is_template_function) return; // dont even attempt to typecheck the body because we dont handle polymorphic templates here!
    
    if (compiler->errors_reported) return;
    
    if (!function->body_checked) {
        function->body_checked = true;
        
        if (function->is_marked_metaprogram) {
            if (function->linkage_name != to_string("main")) {
                compiler->report_error(function, "@metaprogram tag may only be used on the main entry point function.\n");
            }
        }
        
        if (function->is_c_function && function->scope) {
            compiler->report_error(function, "Function marked @c_function cannot have a body.\n");
            return;
        }
        
        if (function->scope) {
            typecheck_scope(function->scope);
        }
        
        compiler->function_emission_queue.add(function);
    }
}
