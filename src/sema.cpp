
#include "sema.h"
#include "ast.h"
#include "compiler.h"
#include "copier.h"
#include "llvm.h"

#include <stdio.h>
#include <new> // for placement new

#ifdef WIN32
#pragma warning(push, 0)
#endif

// For Ast_Os
#include "llvm/Target/TargetMachine.h"

#ifdef WIN32
#pragma warning(pop)
#endif

#define SEMA_NEW(type) (new (compiler->get_memory(sizeof(type))) type())

static bool is_pow2(uint64_t x) {
    return (x & (x - 1)) == 0;
}

static u64 max_integer(int bytesize, bool is_signed) {
    // signed:   127, 32767...
    // unsigned: 255, 65535...
    return (u64(1) << (8 * bytesize - is_signed)) - 1;
}

static void add_type(String_Builder *builder, Ast_Type_Info *type) {
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
        if (type->is_distinct) {
            builder->putchar('D');
            auto alias = type->alias_decl;
            assert(alias);

            // @Incomplete typealiases that are declared within named-scopes.
            String name = alias->identifier->name->name;
            builder->print("%d%.*s", name.length, name.length, name.data);
        }
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
        if (type->is_tuple) {
            builder->putchar('T');
            for (auto mem: type->struct_members) {
                add_type(builder, mem.type_info);
            }
            return;
        }

        builder->putchar('S');

        auto struct_decl = type->struct_decl;
        if (struct_decl->polymorphic_type_alias_scope) {
            for (auto decl: struct_decl->polymorphic_type_alias_scope->declarations) {
                auto type_value = get_type_declaration_resolved_type(decl);
                assert(type_value);
                add_type(builder, type_value);
            }
        }

        // @Incomplete structs that are declared within other structs/named-scopes.
        // @Incomplete anonymous structs?
        String name = type->struct_decl->identifier->name->name;
        builder->print("%d%.*s", name.length, name.length, name.data);
    } else if (type->type == Ast_Type_Info::ENUM) {
        builder->putchar('E');

        // @Incomplete structs that are declared with other structs/named-scopes.
        // @Incomplete anonymous structs?
        String name = type->enum_decl->identifier->name->name;
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
        if (type->is_c_varargs) builder->putchar('V');
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
    } else if (start->owning_function) {
        Ast_Function *function = start->owning_function;

        String name = function->identifier->name->name;
        builder->print("%d%.*s", name.length, name.length, name.data);
        builder->putchar('_');

        for (auto arg: function->arguments) {
            auto type = get_type_info(arg);
            add_type(builder, type);
        }
    }
}

String get_mangled_name(Compiler *compiler, Ast_Function *function) {
    if (function->identifier->name == compiler->atom_main) return function->identifier->name->name;

    // Intrinsics will not implicitly create linkage symbols... at least for now,
    // so we can just return the identifier.
    if (function->is_intrinsic) return function->identifier->name->name;

    String_Builder builder;

    builder.append("_H");

    assert(function->scope);
    maybe_add_parent_scope_name(&builder, function->scope->parent);

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
        if (is_pointer_type(info)) {
            info = info->pointer_to;
            count++;
        } else {
            break;
        }
    }

    return count;
}

bool type_is_iterable(Ast_Type_Info *info) {
    info = get_final_type(info);

    if (info->type == Ast_Type_Info::ARRAY) return true;

    // @Incomplete test for structs containing .count member and supports [] overloading.

    return false;
}

bool type_points_to_void_eventually(Ast_Type_Info *ptr) {
    while (ptr) {
        if (is_pointer_type(ptr)) {
            ptr = ptr->pointer_to;
        } else {
            return get_final_type(ptr)->type == Ast_Type_Info::VOID;
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
        if (info->is_tuple) {
            builder->append("Tuple");
            builder->putchar('(');

            for (array_count_type i = 0; i < info->struct_members.count; ++i) {
                auto mem = info->struct_members[i];
                if (mem.name) builder->print("%.*s: ", PRINT_ARG(mem.name->name));
                print_type_to_builder(builder, mem.type_info);

                if (i+1 < info->struct_members.count) {
                    builder->putchar(',');
                    builder->putchar(' ');
                }
            }

            builder->putchar(')');
            return;
        }

        auto _struct = info->struct_decl;
        maybe_add_struct_parent_name(builder, _struct->member_scope.parent);

        if (_struct->identifier) {
            String name = _struct->identifier->name->name;
            builder->print("%.*s", PRINT_ARG(name));
        }
        return;
    }

    if (info->type == Ast_Type_Info::ALIAS) {
        auto alias = info->alias_decl;

        if (alias->identifier) {
            String name = alias->identifier->name->name;
            builder->print("%.*s ", PRINT_ARG(name));
            builder->putchar('(');
            if (alias->is_distinct) builder->append("@distinct ");
            print_type_to_builder(builder, info->alias_of);
            builder->putchar(')');
        } else {
            print_type_to_builder(builder, info->alias_of);
        }

        return;
    }

    if (info->type == Ast_Type_Info::ENUM) {
        auto _enum = info->enum_decl;

        maybe_add_struct_parent_name(builder, _enum->member_scope.parent);

        if (_enum->identifier) {
            String name = _enum->identifier->name->name;
            builder->print("%.*s", PRINT_ARG(name));
        }
        return;
    }

    if (info->type == Ast_Type_Info::TYPE) {
        // @@ Print type name properly.
        builder->print("<type>");
        return;
    }

    if (info->type == Ast_Type_Info::FUNCTION) {
        // @Incomplete
        builder->putchar('(');
        builder->append("func");
        builder->putchar(')');
        return;
    }

    if (info->type == Ast_Type_Info::TYPE) {
        builder->append("Type");
        return;
    }

    assert(false);
}

String type_to_string(Ast_Type_Info *info) {
    String_Builder builder;
    print_type_to_builder(&builder, info);
    return builder.to_string();
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

            if (!deref->is_type_dereference && is_pointer_type(get_type_info(deref->left))) {
                return true;
            }

            return expression_is_lvalue(deref->left, parent_wants_lvalue);
        }

        case AST_ARRAY_DEREFERENCE: {
            // auto deref = static_cast<Ast_Dereference *>(expression);
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
            } else if (un->operator_type == Token::MINUS) {
                return false;
            } else if (un->operator_type == Token::EXCLAMATION) {
                return false;
            } else {
                assert(false && "Unhandled Unary Expression type in expression_is_lvalue.");
                return false;
            }
        }

        case AST_TUPLE_EXPRESSION: {
            // If all arguments of a tuple expression are lvalues, then so is the tuple.
            auto tuple = static_cast<Ast_Tuple_Expression *>(expression);

            for (auto arg: tuple->arguments) {
                if (!expression_is_lvalue(arg, parent_wants_lvalue)) return false;
            }

            return true;
        }

        default:
            return false;
    }
}

static bool expression_needs_enum_type_inference(Ast_Expression * expr) {
    if (expr->type == AST_DEREFERENCE) {
        auto deref = static_cast<Ast_Dereference*>(expr);
        return deref->left == nullptr;
    }
    return false;
}

// @@ Should this be an atribute of the literal that is propagated? Is 1+1 mutable?
static bool is_mutable_literal(Ast_Literal * literal) {
    if (literal->type_info->type == Ast_Type_Info::INTEGER || literal->type_info->type == Ast_Type_Info::FLOAT) {
        return true;
    }
    if (literal->type_info->type == Ast_Type_Info::POINTER) {
        return literal->literal_type == Ast_Literal::NULLPTR;
    }
    return false;
}

// @Returns viability contribution
u64 maybe_mutate_literal_to_type(Ast_Literal *lit, Ast_Type_Info *target_type) {
    target_type = get_final_type(target_type);
    assert(target_type != nullptr);

    u64 viability_score = 0;
    if (lit->literal_type == Ast_Literal::INTEGER) {
        if (is_int_or_enum_type(target_type) || is_float_type(target_type)) {
            if (!types_match(get_type_info(lit), target_type)) {
                viability_score += 1;
            }
            auto old_type = lit->type_info;

            if (is_int_or_enum_type(target_type)) {
                auto target = target_type;
                if (is_enum_type(target_type)) target = target_type->enum_base_type;

                // I have disabled this stuff for the time being because it prevents the user from being able to do:
                // let MY_UNSIGNED_VALUE: uint32 = -1;
                // which is completely valid code, a cast should not be necessary. -josh 4 February 2020

                /*
                // Check that number can fit in target type
                if (target->is_signed) {
                    s64 min, max;
                    if (target->size == 1) { min = INT8_MIN; max = INT8_MAX; }
                    else if (target->size == 2) { min = INT16_MIN; max = INT16_MAX; }
                    else if (target->size == 4) { min = INT32_MIN; max = INT32_MAX; }
                    else { assert(target->size == 8); min = INT64_MIN; max = INT64_MAX; }

                    s64 x = lit->integer_value;
                    if (x > max || x < min) return viability_score; // @@ Doest it matter what we return in this case?
                }
                else {
                    u64 max;
                    if (target->size == 1) { max = UINT8_MAX; }
                    else if (target->size == 2) { max = UINT16_MAX; }
                    else if (target->size == 4) { max = UINT32_MAX; }
                    else { assert(target->size == 8); max = UINT64_MAX; }

                    u64 x = (u64)lit->integer_value;
                    if (x > max) return viability_score; // @@ Doest it matter what we return in this case?
                }
                */
            }
            else if (is_float_type(target_type)) {
                // @@ Check that integer can be represented exactly with a float.
            }
            
            // @Incomplete cast to float if we have an int literal
            lit->type_info = target_type;

            // @Cleanup I'm mutating the literal for now, but this would be a good place to use substitution, I think
            // Or since literal ints are considered completely typeless up until this point, maybe this is the right thing to do
            if (is_float_type(target_type)) {
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
    else if (lit->literal_type == Ast_Literal::FLOAT) {
        if (target_type && is_float_type(target_type)) {
            if (!types_match(get_type_info(lit), target_type)) viability_score += 1;
            lit->type_info = target_type;
        }
        //else lit->type_info = compiler->type_float64; // @TODO we should probably have a check that verifies if the literal can fit in a 32-bit float and then default to that.
    }

    else if (lit->literal_type == Ast_Literal::NULLPTR) {
        if (is_pointer_type(target_type)) {
            lit->type_info = target_type;
        } else if (is_function_type(target_type)) {
            lit->type_info = target_type;
        }
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

    /*if (auto lit = folds_to_literal(expression)) {
        auto score = maybe_mutate_literal_to_type(lit, target_type_info);
        expression = lit;
        viability_score += score;
    }*/

    auto lit = folds_to_literal(expression);
    if (lit) {
        if (is_mutable_literal(lit)) {
            auto score = maybe_mutate_literal_to_type(lit, target_type_info);
            expression = lit;
            //expression->substitution = lit;
            viability_score += score;
        }
        else {
            // We fold expressions even if they cannot be mutated.
            expression = lit;
            //expression->substitution = lit;
        }
    }


    auto rtype = get_final_type(get_type_info(expression));
    auto ltype = get_final_type(target_type_info);

    auto right = expression;

    if (!types_match(ltype, rtype)) {
        if (ltype->type == Ast_Type_Info::ALIAS) {
            assert(ltype->is_distinct);
            // If the types do not match and it is because ltype is distinct,
            // we cannot proceed further with trying to cast this expression.
            return MakeTuple(viability_score, right);
        }

        if (rtype->type == Ast_Type_Info::ALIAS) {
            assert(rtype->is_distinct);

            rtype = get_underlying_final_type(rtype);
            // insert a cast so the rest of the implicit casting works correctly.
            // @Cutnpaste from cast_int_to_int
            Ast_Cast *cast = SEMA_NEW(Ast_Cast);
            copy_location_info(cast, right);
            cast->expression = right;
            cast->type_info = rtype;

            right = cast;
            viability_score += 1;
        }

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
        } else if (is_pointer_type(ltype) && is_pointer_type(rtype)) {

            auto left_indir = get_levels_of_indirection(ltype);
            auto right_indir = get_levels_of_indirection(rtype);

            // @Note you're only allowed to coerce right-to-left here, meaning if the right-expression is *void, the left-expression cannot coerce away from whatever ptr type it is.
            if ((allow_flags & ALLOW_COERCE_TO_PTR_VOID) && type_points_to_void_eventually(ltype)) {
                if (left_indir == right_indir) {
                    right = cast_ptr_to_ptr(compiler, right, ltype);
                }
            }

            if (is_struct_type(ltype->pointer_to) && is_struct_type(rtype->pointer_to)) {
                auto target = ltype->pointer_to;
                auto source = rtype->pointer_to->parent_struct;

                while (source) {
                    if (types_match(target, source)) {
                        right = cast_ptr_to_ptr(compiler, right, ltype);
                        break;
                    }

                    source = get_final_type(source)->parent_struct;
                }
            }

            viability_score += 1;
        } else if (ltype->type == Ast_Type_Info::BOOL && (allow_flags & ALLOW_COERCE_TO_BOOL)) {
            if (is_pointer_type(rtype) || rtype->type == Ast_Type_Info::FUNCTION) {
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
        } else if (is_pointer_type(ltype)
                && (types_match(ltype->pointer_to, compiler->type_uint8) || types_match(ltype->pointer_to, compiler->type_int8))
                && types_match(rtype, compiler->type_string)) {
            // Implicity dereference string when the left-type is *uint8 or *int8.
            // @TODO maybe have an allow parameter for this in case this should only be allowed in certain situations.

            // Only allow this for literals because this is a convinience for passing literal strings to C functions!
            if (auto literal = folds_to_literal(expression)) {
                auto deref = make_dereference(compiler, literal, compiler->atom_data);
                typecheck_expression(deref);

                right = deref;
                viability_score += 1;

                if (!types_match(ltype, compiler->type_string_data)) {
                    // Add a pointer cast to the target type if it is *int8, or otherwise not the internal type of string.data.
                    auto cast = cast_ptr_to_ptr(compiler, deref, ltype);
                    typecheck_expression(cast);

                    // We don't penalize the viability for this. You get this as a courtesy.
                    right = cast;
                }
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

    if (expression->type == AST_LITERAL) {
        // @FixMe maybe, we're returning a copy here because if function-call typechecking mutates
        // the literal, it will try to mutate the literal multiple times while checking two or more
        // overloads. This means that without a copy, we end up messing up viability scores on subsequent
        // overload checks. -josh 21 December 2019
        auto lit = static_cast<Ast_Literal *>(compiler->copier->copy(expression));
        lit->type_info = get_type_info(expression);
        return lit;
    }

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

            if (left_type->type == Ast_Type_Info::INTEGER || left_type->type == Ast_Type_Info::ENUM) {
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

                    default:
                        assert(false && "Unhandled binary operator in folds_to_literal.");
                        return nullptr;
                }
            } else if (left_type->type == Ast_Type_Info::FLOAT) {
                double l = left->float_value;
                double r = right->float_value;

                switch (bin->operator_type) {
                    case Token::PLUS : return make_float_literal(compiler, l + r, left_type, bin);
                    case Token::MINUS: return make_float_literal(compiler, l - r, left_type, bin);
                    case Token::STAR : return make_float_literal(compiler, l * r, left_type, bin);
                    case Token::SLASH: return make_float_literal(compiler, l / r, left_type, bin);

                    case Token::LE_OP: return make_bool_literal(compiler, l <= r, bin);
                    case Token::GE_OP: return make_bool_literal(compiler, l >= r, bin);
                    case Token::EQ_OP: return make_bool_literal(compiler, l == r, bin);
                    case Token::NE_OP: return make_bool_literal(compiler, l != r, bin);
                    case Token::LEFT_ANGLE : return make_bool_literal(compiler, l <  r, bin);
                    case Token::RIGHT_ANGLE: return make_bool_literal(compiler, l >  r, bin);

                    default:
                        assert(false && "Unhandled binary operator in folds_to_literal.");
                        return nullptr;
                }
            } else if (left_type->type == Ast_Type_Info::BOOL) {
                s64 left_bool  = left->bool_value;
                s64 right_bool = right->bool_value;
                switch (bin->operator_type) {
                    case Token::OR_OP : FOLD_COMPARE(||, left_bool, right_bool, left_type, bin);
                    case Token::AND_OP: FOLD_COMPARE(&&, left_bool, right_bool, left_type, bin);
                    case Token::EQ_OP : FOLD_COMPARE(==, left_bool, right_bool, left_type, bin);
                    case Token::NE_OP : FOLD_COMPARE(!=, left_bool, right_bool, left_type, bin);

                    default:
                        assert(false && "Unhandled binary operator in folds_to_literal.");
                        return nullptr;
                }
            } else if (left_type->type == Ast_Type_Info::TYPE) {
                #if 0 
                    // This relies on the table index for type comparison, which is the same we currently do at runtime.
                    s64 left_int  = left->integer_value;
                    s64 right_int = right->integer_value;
                    switch (bin->operator_type) {
                        case Token::EQ_OP: return make_bool_literal(compiler, left_int == right_int, bin);
                        case Token::NE_OP: return make_bool_literal(compiler, left_int != right_int, bin);
                        default:
                            assert(false && "Unexpected binary operator in folds_to_literal.");
                            return nullptr;
                    }
                #else
                    // Instead of the type index, use the type value at that index. Note, that at runtime we are still
                    // comparing the indices directly.

                    auto left_type = compiler->type_table[left->integer_value];
                    auto right_type = compiler->type_table[right->integer_value];

                    bool match = types_match(left_type, right_type);
                    switch (bin->operator_type) {
                        case Token::EQ_OP: return make_bool_literal(compiler, match, bin);
                        case Token::NE_OP: return make_bool_literal(compiler, !match, bin);
                        default:
                            assert(false && "Unexpected binary operator in folds_to_literal.");
                            return nullptr;
                    }
                #endif
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
                auto left_type = get_final_type(get_type_info(un));

                assert(types_match(left_type, get_type_info(rhs)));
                if (is_int_type(left_type)) {
                    assert(rhs->literal_type == Ast_Literal::INTEGER);
                    // @Incomplete if we need to work about casting between the target type sizes and s64, the we probably need to do that here too.
                    return make_integer_literal(compiler, -rhs->integer_value, left_type, un);
                } else if (is_float_type(left_type)) {
                    assert(rhs->literal_type == Ast_Literal::FLOAT);
                    return make_float_literal(compiler, -rhs->float_value, left_type, un);
                } else {
                    return nullptr;
                }
            }

            // @Incomplete unary bool-not, unary bitwise-not

            return nullptr;
        }

        case AST_IDENTIFIER: {
            auto ident = static_cast<Ast_Identifier *>(expression);
            auto decl = ident->resolved_declaration;

            if (decl->type == AST_DECLARATION) {
                return folds_to_literal(static_cast<Ast_Declaration *>(decl));
            }
            if (decl->type == AST_TYPE_ALIAS) {
                auto alias = static_cast<Ast_Type_Alias *>(decl);
                auto literal = make_integer_literal(compiler, alias->type_value->type_table_index, compiler->type_info_type, decl);
                return literal;
            }
            if (decl->type == AST_STRUCT) {
                auto _struct = static_cast<Ast_Struct *>(decl);
                auto literal = make_integer_literal(compiler, _struct->type_value->type_table_index, compiler->type_info_type, decl);
                return literal;
            }
            if (decl->type == AST_ENUM) {
                auto _enum = static_cast<Ast_Enum *>(decl);
                auto literal = make_integer_literal(compiler, _enum->type_value->type_table_index, compiler->type_info_type, decl);
                return literal;
            }
            if (decl->type == AST_FUNCTION) {
                auto func = static_cast<Ast_Function *>(decl);
                auto literal = make_function_literal(compiler, func, ident);
                return literal;
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
                    // Why are we changing the type of the declaration? This is producing incorrect results when
                    // the declaration is an enum that's initialized with an integer expression.
                    // @@ Is that really what's happening?
                    literal->type_info = get_type_info(decl->initializer_expression);
                }
                return literal;
            }

            return nullptr;
        }

        case AST_TYPE_INSTANTIATION: {
            auto type_inst = static_cast<Ast_Type_Instantiation *>(expression);
            
            auto literal = make_integer_literal(compiler, type_inst->type_value->type_table_index, compiler->type_info_type, type_inst);

            return literal;
        }

        case AST_CAST: {
            auto cast = static_cast<Ast_Cast *>(expression);
            auto target_type = get_final_type(cast->type_info);

            //Ast_Type_Instantiation *target_type_inst = nullptr;
            // Ast_Expression *expression = nullptr;

            auto literal = folds_to_literal(cast->expression);
            if (literal) {
                assert(literal->type == AST_LITERAL);

                // @@ Should we fold this with mutate_literal_to_type()

                if (is_int_type(target_type)) {
                    if (is_int_type(literal->type_info)) {
                        return make_integer_literal(compiler, literal->integer_value, target_type);
                    }
                    else if (is_float_type(literal->type_info)) {
                        return make_integer_literal(compiler, literal->float_value, target_type);
                    }
                    else if (get_final_type(literal->type_info)->type == Ast_Type_Info::BOOL) {
                        return make_integer_literal(compiler, literal->bool_value, target_type);
                    }
                    else if (is_enum_type(literal->type_info)) {
                        return make_integer_literal(compiler, literal->integer_value, target_type);
                    }
                }
                else if (is_float_type(target_type)) {
                    if (is_int_type(literal->type_info)) {
                        return make_float_literal(compiler, literal->integer_value, target_type);
                    }
                    else if (is_float_type(literal->type_info)) {
                        // @@ Should we avoid doing anything in this case?
                        return make_float_literal(compiler, literal->float_value, target_type);
                    }
                    else if (get_final_type(literal->type_info)->type == Ast_Type_Info::BOOL) {
                        return make_float_literal(compiler, literal->bool_value, target_type);
                    }
                }
                else if (target_type->type == Ast_Type_Info::BOOL) {
                    if (is_int_type(literal->type_info)) {
                        return make_bool_literal(compiler, literal->integer_value != 0);
                    }
                    else if (is_float_type(literal->type_info)) {
                        return make_bool_literal(compiler, literal->float_value != 0.0);
                    }
                    else if (get_final_type(literal->type_info)) {
                        return literal;
                    }
                }
                else if (is_enum_type(target_type)) {
                    if (is_int_type(literal->type_info)) {
                        return make_integer_literal(compiler, literal->integer_value, target_type);
                    }
                    else if (is_enum_type(literal->type_info)) {
                        return make_integer_literal(compiler, literal->integer_value, target_type);
                    }
                }
                else {
                    return nullptr;
                }
            }

            return nullptr; // @@ Other casts not suported yet.
        }

        default: return nullptr;
    }
}

// @Cleanup if we introduce expression substitution, then we can remove the result parameters and just set (left/right)->substitution
// Actually, if we do that, then we can't really use this for checking function calls.
// Actually, this is being deprecated for things other than binary operators and for-loop ranges... since function calls now use typecheck_and_implicit_cast_single_expression, declarations need only do one-way casting, returns are one-way.
// Perhaps also, we should break this out into several calls to typecheck_and_implicit_cast_single_expression... -josh 21 July 2019
Tuple<u64, u64> Sema::typecheck_and_implicit_cast_expression_pair(Ast_Expression *left, Ast_Expression *right, Ast_Expression **result_left, Ast_Expression **result_right, bool allow_coerce_to_ptr_void) {
    
    auto lit_left = folds_to_literal(left);
    if (compiler->errors_reported) return MakeTuple<u64, u64>(0, 0);

    auto lit_right = folds_to_literal(right);
    if (compiler->errors_reported) return MakeTuple<u64, u64>(0, 0);

    // @@ Shouldn't we do substitution regardless of whether we mutate the type?
    // I think we do, otherwise in expressions such as `day == Day.Tuesday` the right hand side is never folded.
    if (lit_left) {
        left->substitution = lit_left;
    }
    if (lit_right) {
        right->substitution = lit_right;
    }

    if (lit_left && is_mutable_literal(lit_left)) {
        maybe_mutate_literal_to_type(lit_left, get_type_info(right));
        left = lit_left;
        //left->substitution = lit_left;
    }
    else if (lit_right && is_mutable_literal(lit_right)) {
        maybe_mutate_literal_to_type(lit_right, get_type_info(left));
        right = lit_right;
        //right->substitution = lit_right;
    }

    // Original code:
    /*
    if (auto lit = folds_to_literal(left)) {
        typecheck_expression(right);
        if (compiler->errors_reported) return MakeTuple<u64, u64>(0, 0);

        // typecheck_expression(left, get_type_info(right));
        assert(get_type_info(lit));
        maybe_mutate_literal_to_type(lit, get_type_info(right));
        left = lit;
    } else if (auto lit = folds_to_literal(right)) {
        typecheck_expression(left);
        if (compiler->errors_reported) return MakeTuple<u64, u64>(0, 0);

        // typecheck_expression(right, get_type_info(left));
        assert(get_type_info(lit));
        maybe_mutate_literal_to_type(lit, get_type_info(left));
        right = lit;
    } else {
        typecheck_expression(left);
        if (compiler->errors_reported) return MakeTuple<u64, u64>(0, 0);

        typecheck_expression(right, get_type_info(left));
    }
    */

    if (compiler->errors_reported) return MakeTuple<u64, u64>(0, 0);

    while (left->substitution)  left  = left->substitution;
    while (right->substitution) right = right->substitution;

    assert(left->type_info);
    assert(right->type_info);

    auto ltype = get_final_type(get_type_info(left));
    auto rtype = get_final_type(get_type_info(right));
    u64  left_viability_score  = 0;
    u64  right_viability_score = 0;

    defer {
        if (result_left)  *result_left  = left;
        if (result_right) *result_right = right;
    };

    if (!types_match(ltype, rtype)) {

        if (ltype->type == Ast_Type_Info::ALIAS && rtype->type == Ast_Type_Info::ALIAS) {
            assert(rtype->is_distinct);
            assert(ltype->is_distinct);
            // If the types do not match and it is because both types are distinct
            // we cannot cast away from either.
            MakeTuple(left_viability_score, right_viability_score);
        }

        if (rtype->type == Ast_Type_Info::ALIAS) {
            assert(rtype->is_distinct);

            rtype = get_underlying_final_type(rtype);
            // insert a cast so the rest of the implicit casting works correctly.
            // @Cutnpaste from cast_int_to_int
            Ast_Cast *cast = SEMA_NEW(Ast_Cast);
            copy_location_info(cast, right);
            cast->expression = right;
            cast->type_info = rtype;

            right = cast;
            right_viability_score += 1;
        } else if (ltype->type == Ast_Type_Info::ALIAS) {
            assert(ltype->is_distinct);

            ltype = get_underlying_final_type(ltype);
            // insert a cast so the rest of the implicit casting works correctly.
            // @Cutnpaste from cast_int_to_int
            Ast_Cast *cast = SEMA_NEW(Ast_Cast);
            copy_location_info(cast, left);
            cast->expression = left;
            cast->type_info = ltype;

            left = cast;
            left_viability_score += 1;
        }

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
        } else if (allow_coerce_to_ptr_void && is_pointer_type(ltype) && is_pointer_type(rtype)) {

            // @Note you're only allowed to coerce right-to-left here, meaning if the right-expression is *void,
            // the left-expression cannot coerce away from whatever ptr type it is.
            // UPDATE: I am not sure this is true in the general case. This should probably work in both directions.
            // -josh 27 November 2019
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

    return MakeTuple(left_viability_score, right_viability_score);
}

void Sema::typecheck_scope(Ast_Scope *scope) {
    assert(scope->substitution == nullptr);

    for (auto &it : scope->statements) {
        // @TODO should we do replacements at the scope level?
        typecheck_expression(it, nullptr, /*overload_set_allowed*/false, /*do_function_body*/true, /*only_want_struct_type*/false);

        if (compiler->errors_reported) return;
    }
}

Ast_Function *Sema::get_polymorph_for_function_call(Ast_Function *template_function, Ast_Function_Call *call, bool do_errors) {
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

    auto result = compiler->copier->polymorph_function_with_arguments(template_function, &call->argument_list, call->implicit_argument_inserted, call, do_errors);
    auto polymorph   = result.item1;
    bool is_existing = result.item2;
    if (polymorph) {
        if (!is_existing) {
            // Add polypmorph to polymorphed_overloads first so we do not cause in infinite loop
            // in some cases.
            template_function->polymorphed_overloads.add(polymorph);
            typecheck_function(polymorph);
        }
    }
    if (compiler->errors_reported) return nullptr;

    if (polymorph) assert(!polymorph->is_template_function);
    return polymorph;
}

Ast_Function *Sema::get_best_overload_from_set(Ast_Function_Call *call, Array<Ast_Function *> &overload_set) {
    Ast_Function *function = nullptr;
    // @Cleanup I'm not sure why I did this, but we can probably merge this with the general case for multiple overloads.
    if (overload_set.count == 1) {
        function = overload_set[0];
        typecheck_function_header(function);
        if (compiler->errors_reported) return nullptr;

        if (function->is_template_function) {
            function = get_polymorph_for_function_call(function, call, false);
            if (compiler->errors_reported) return nullptr;

            if (!function) return nullptr;
        }

        auto tuple = function_call_is_viable(call, get_type_info(function), function, false);
        if (compiler->errors_reported) return nullptr;

        bool viable = tuple.item1;
        if (!viable) return nullptr;
    } else {
        const u64 U64_MAX = 0xFFFFFFFFFFFFFFFF;
        u64 lowest_score = U64_MAX;
        for (auto overload : overload_set) {
            if (overload->is_template_function) {
                overload = get_polymorph_for_function_call(overload, call, false);
                if (compiler->errors_reported) return nullptr;

                if (!overload) continue; // no polymorphs that match this call, so skip it
            }

            typecheck_function_header(overload);
            if (compiler->errors_reported) return nullptr;

            auto tuple = function_call_is_viable(call, get_type_info(overload), overload, false);
            if (compiler->errors_reported) return nullptr;

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

    return function;
}

Tuple<bool, u64> Sema::function_call_is_viable(Ast_Function_Call *call, Ast_Type_Info *function_type, Ast_Function *source, bool perform_full_check) {
    assert(function_type);
    assert(function_type->type == Ast_Type_Info::FUNCTION);

    bool pass_c_varags = (function_type->is_c_varargs && call->argument_list.count >= function_type->arguments.count);

    auto old_call_count = call->argument_list.count; // This feels like a bit of a @Hack
    defer { if (!perform_full_check) call->argument_list.count = old_call_count; };

    u64 viability_score = 0;

    // Insert default arguments from source function if able.
    if (source && call->argument_list.count < source->arguments.count) {
        for (array_count_type i = call->argument_list.count; i < source->arguments.count; ++i) {
            auto arg = source->arguments[i];

            if (arg->initializer_expression) {
                // @Cleanup this folds_to_literal is likely unnecessary since we can typecheck this expression once in typecheck_function_header
                // and it will not need to mutate its type or value, maybe.
                auto lit = folds_to_literal(arg->initializer_expression);
                assert(lit);

                call->argument_list.add(lit);
                viability_score += 1;
            }
        }
    }

    if (!pass_c_varags && call->argument_list.count != function_type->arguments.count) {
        // @TODO print function declaration as well as call site
        if (perform_full_check) {
            compiler->report_error(call, "Mismatch in function call arguments. Wanted %lld, got %lld.\n", function_type->arguments.count, call->argument_list.count);
        }
        return MakeTuple<bool, u64>(false, 0);
    }

    for (array_count_type i = 0; i < call->argument_list.count; ++i) {
        auto original_value = call->argument_list[i];
        auto value = original_value;

        // Handle implicit argument conversions.
        if (call->implicit_argument_inserted && function_type->arguments.count && i == 0) {
            Ast_Expression *source = call->argument_list[0];

            auto source_type = get_type_info(source);
            auto param_type  = function_type->arguments[0];

            if (!types_match(source_type, param_type)) {
                bool is_struct_or_array = is_struct_type(source_type) || is_array_type(source_type);
                bool is_param_struct_or_array = is_struct_type(param_type) || is_array_type(param_type);
                if (is_struct_or_array && is_pointer_type(param_type)) {
                    // Turn this struct into a pointer
                    auto unary = make_unary(compiler, Token::STAR, source);
                    // @Speed we can probably just assign the right type here.
                    typecheck_expression(unary);

                    value = unary;
                    // We still add a viability score increment in case the user has two functions
                    // that overload between the const-ref and pointer types.
                    viability_score += 1;
                } else if (is_pointer_type(source_type) && is_param_struct_or_array) {
                    // Turn this into a dereference
                    auto unary = make_unary(compiler, Token::DEREFERENCE_OR_SHIFT, source);
                    // @Speed we can probably just assign the right type here.
                    typecheck_expression(unary);

                    value = unary;
                    // We still add a viability score increment in case the user has two functions
                    // that overload between the const-ref and pointer types.
                    viability_score += 1;
                }
            }
        }

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

            if (auto lit = folds_to_literal(value)) {
                //auto score = maybe_mutate_literal_to_type(lit, target_type_info);
                value = lit;
            }

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
        assert(it->substitution == nullptr);
        // while (it->substitution) it = it->substitution;

        if (!check_private_declarations && (it->declaration_flags & DECLARATION_IS_PRIVATE)) continue;

        if (it->type == AST_FUNCTION) {
            auto function = static_cast<Ast_Function *>(it);
            if (function->identifier->name == atom) {
                // printf("Adding overlaod: %p\n", function);
                overload_set->add(function);
            }
        } else if (it->type == AST_SCOPE_EXPANSION) {
            auto exp = reinterpret_cast<Ast_Scope_Expansion *>(it);

            bool check_private = (exp->expanded_via_import_directive == nullptr);
            collect_function_overloads_for_atom_in_scope(atom, exp->scope, overload_set, check_private);
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
        assert(it->substitution == nullptr);
        // while (it->substitution) it = it->substitution;

        if (!check_private_declarations && (it->declaration_flags & DECLARATION_IS_PRIVATE)) continue;

        if (is_declaration(it->type)) {
            if (it->identifier && it->identifier->name == atom) return it;
        } else if (it->type == AST_SCOPE_EXPANSION) {
            auto exp = static_cast<Ast_Scope_Expansion *>(it);

            bool check_private = (exp->expanded_via_import_directive == nullptr);
            auto decl = find_declaration_for_atom_in_scope(exp->scope, atom, check_private);
            if (decl) return decl;
        } else {
            assert(false);
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

static
Ast_Expression *get_nearest_owner(Ast_Scope *scope) {
    while (scope) {
        if      (scope->owning_statement) return scope->owning_statement;
        else if (scope->owning_enum)      return scope->owning_enum;
        else if (scope->owning_function)  return scope->owning_function;
        else if (scope->owning_struct)    return scope->owning_struct;

        scope = scope->parent;
    }
    return nullptr;
}

static
bool check_type_info_for_struct_member (Compiler *compiler, Ast_Dereference *deref, Ast_Type_Info *struct_type, Atom *field_atom) {        
    while (struct_type) {
        for (auto member : struct_type->struct_members) {
            if (member.is_anonymous_struct) {
                // @Hack @FixMe We generate this stuff first otherwise if we find a member that is within a nested
                // anonymous struct, we end up having our dereferences in reverse order and cause errors in the LLVM generator.
                // If we found a candidate, we need to insert a dereference through this member.
                Ast_Dereference *deref_through_anon = SEMA_NEW(Ast_Dereference);
                deref_through_anon->left = deref->left; // @Hack so expression_is_lvalue doesnt crash
                deref_through_anon->element_path_index = member.element_index;
                deref_through_anon->type_info = member.type_info;
                deref_through_anon->byte_offset = -1; // @Incomplete

                assert(deref_through_anon->type_info);

                auto old_left = deref->left;
                deref->left = deref_through_anon;

                bool found = check_type_info_for_struct_member(compiler, deref, member.type_info, field_atom);
                if (!found) {
                    deref->left = old_left;
                    continue;
                }
                
                return true;
            }

            if (member.name == field_atom) {
                deref->element_path_index = member.element_index;
                deref->type_info = member.type_info;
                deref->byte_offset = -1; // @Incomplete
                return true;
            }
        }

        struct_type = struct_type->parent_struct;
    }

    return false;
}

s64 pad_to_alignment(s64 current, s64 align) {
    assert(align >= 1);

    s64 minum = current & (align-1);
    if (minum) {
        assert((current % align) != 0);
        current += align - minum;
    }

    return current;
}

void Sema::typecheck_expression(Ast_Expression *expression, Ast_Type_Info *want_numeric_type, bool overload_set_allowed, bool do_function_body, bool only_want_struct_type) {
    while (expression->substitution) expression = expression->substitution;

    // @Temporary maybe, if this is a function declaration, typecheck it anyways since typecheck_function_header() will have set
    // the type info on the function node, but not have checked the entire body.
    if (expression->type != AST_FUNCTION && expression->type != AST_STRUCT && expression->type_info) return;
    if (expression->type == AST_FUNCTION && static_cast<Ast_Function *>(expression)->body_checked) return;
    if (expression->type == AST_STRUCT   && static_cast<Ast_Struct *>(expression)->member_scope.type_info) return;

    switch (expression->type) {
        case AST_UNINITIALIZED: {
            assert(false && "Unitialized AST Node!");
            return;
        }
        case AST_TYPE_INSTANTIATION: {
            // We should not get here due to the fact that this currently do not organically
            // call typecheck_expression on Ast_Type_Instantiation because it is normally
            // only available attached to other AST nodes, which will call resolve_type_inst
            // on it.
            //assert(false);

            resolve_type_inst(static_cast<Ast_Type_Instantiation*>(expression));
            expression->type_info = compiler->type_info_type;

            if (auto lit = folds_to_literal(expression)) {
                expression->substitution = lit;
                //expression = lit;
            }

            return;
        }
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
        case AST_DIRECTIVE_CLANG_IMPORT: {
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
                compiler->report_error(ident, "Undeclared identifier '%.*s'\n", PRINT_ARG(name));
            } else {
                if (overload_set_allowed) {
                    assert(ident->overload_set.count == 0);
                    collect_function_overloads_for_atom(ident->name, ident->enclosing_scope, &ident->overload_set);

                    // resolved_declaration and type_info will be resolved by the Ast_Function_Call code.
                    // Set to void for now, Ast_Function_Call code will either error or fix this up.
                    ident->type_info = compiler->type_void;
                } else if (decl->type == AST_FUNCTION) {
                    assert(ident->overload_set.count == 0);
                    collect_function_overloads_for_atom(ident->name, ident->enclosing_scope, &ident->overload_set);

                    if (ident->overload_set.count > 1) {
                        String name = ident->name->name;
                        compiler->report_error(ident, "Ambiguous use of overloaded function '%.*s' (%d overloads).\n", name.length, name.data, ident->overload_set.count);


                        for (auto overload: ident->overload_set) {
                            compiler->report_error(overload, "DEBUG: here\n");
                        }

                        return;
                    } else {
                        assert(ident->overload_set.count == 1);

                        typecheck_expression(decl);
                        ident->resolved_declaration = decl;
                        ident->type_info = get_type_info(decl);
                        return;
                    }

                    // resolved_declaration and type_info will be resolved by the Ast_Function_Call code.
                    // Set to void for now, Ast_Function_Call code will either error or fix this up.
                    ident->type_info = compiler->type_void;
                    return;
                }

                {
                    typecheck_expression(decl);
                    ident->resolved_declaration = decl;
                    ident->type_info = get_type_info(decl);

                    if (decl->type == AST_DECLARATION) {
                        auto declaration = static_cast<Ast_Declaration *>(decl);

                        if (declaration->is_struct_member && !declaration->is_let) {
                            // @TODO We usually get here if someone tries to use a struct-member-variable
                            // from within a function declared within the struct. Usually, these functions
                            // will almost always be typechecked before we get here, but that may not always
                            // be the case. We will want to set is_struct_member in the Ast_Declaration type-
                            // checking code. -josh 28 Janurary 2020
                            compiler->report_error(ident, "Attempt to use struct variable member without an instance!\n");
                            return;
                        }

                        if (!declaration->is_let) {
                            auto owner = get_nearest_owner(ident->enclosing_scope);
                            auto decl_owner = get_nearest_owner(declaration->identifier->enclosing_scope);

                            if (owner && owner->type == AST_FUNCTION && decl_owner && decl_owner->type == AST_FUNCTION && owner != decl_owner) {
                                compiler->report_error(ident, "Attempt to use a variable from an outer stack frame.\n");
                                compiler->report_error(declaration, "Variable declared here.\n");
                                return;
                            }
                        }
                    }
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
                auto decl_type = get_type_info(decl);

                if (decl_type) {
                    auto result = typecheck_and_implicit_cast_single_expression(decl->initializer_expression, decl_type, 0);
                    decl->initializer_expression = result.item2; // @Cleanup use substitution if possible
                } else {
                    typecheck_expression(decl->initializer_expression);
                }

                if (compiler->errors_reported) return;
            }
            
            if (decl->is_let && !decl->is_readonly_variable && !decl->initializer_expression && !decl->is_enum_member) {
                compiler->report_error(decl, "let constant must be initialized by an expression.\n");
                return;
            }

            if (decl->is_let && !decl->is_readonly_variable && decl->initializer_expression) {
                auto literal = folds_to_literal(decl->initializer_expression);

                if (!literal) {
                    compiler->report_error(decl->initializer_expression, "let constant may only be initialized by a literal expression.\n");
                    return;
                } else {
                    if (decl->initializer_expression != literal) decl->initializer_expression->substitution = literal;
                }

                // decl->substitution = decl->initializer_expression;
            }

            if (!decl->is_let && decl->is_struct_member && decl->initializer_expression) {
                if (!folds_to_literal(decl->initializer_expression)) {
                    compiler->report_error(decl->initializer_expression, "Struct member may only be initialized by a literal expression.\n");
                    return;
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

            if (decl->type_info && decl->initializer_expression) {
                auto result = typecheck_and_implicit_cast_single_expression(decl->initializer_expression, get_type_info(decl), ALLOW_COERCE_TO_PTR_VOID);
                if (compiler->errors_reported) return;

                if (decl->initializer_expression != result.item2) decl->initializer_expression = result.item2; // Don't do a substitution here, otherwise we can cause a loop
                
                if (!types_match(get_type_info(decl), get_type_info(decl->initializer_expression))) {
                    // @@ The error message is good, but the error highlight could be improved, currently it points to the ':' in the declaration only.
                    // Note that due to the above substitution it's not a good idea to point to the initializer.
                    auto wanted = type_to_string(get_type_info(decl));
                    auto given  = type_to_string(get_type_info(decl->initializer_expression));
                    compiler->report_error(decl, "Attempt to initialize '%.*s' with expression of incompatible type (Wanted %.*s, Given %.*s).\n",
                                           PRINT_ARG(decl->identifier->name->name), PRINT_ARG(wanted), PRINT_ARG(given));
                    free(wanted.data);
                    free(given.data);
                    return;
                }
            }

            if (!decl->is_let && decl->identifier && compiler->is_toplevel_scope(decl->identifier->enclosing_scope)) {
                if (decl->initializer_expression && !resolves_to_literal_value(decl->initializer_expression)) {
                    compiler->report_error(decl, "Global variable may only be initialized by a literal expression.\n");
                }

                compiler->global_decl_emission_queue.add(decl);
            }

            if (compiler->errors_reported) return;

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
                operation->enclosing_scope = bin->enclosing_scope;
                auto assignment = make_binary(compiler, Token::EQUALS, bin->left, operation, bin);
                assignment->enclosing_scope = bin->enclosing_scope;

                bin->substitution = assignment;
                bin = assignment;
            }

            // We handled operator[]= before typechecking anything otherwise we run the risk of
            // The Ast_Array_Dereference resolving to an overload of operator[]
            if (bin->operator_type == Token::EQUALS) {
                if (bin->left->type == AST_ARRAY_DEREFERENCE) {
                    Atom *operator_atom = compiler->make_atom(OPERATOR_BRACKET_EQUALS_NAME);
                    auto arr_deref = static_cast<Ast_Array_Dereference *>(bin->left);

                    Ast_Identifier *ident = SEMA_NEW(Ast_Identifier);
                    ident->name = operator_atom;

                    // First we check in the type on the left if an overload is available.
                    // This should save a lot of time compared to doing a full scope tree lookup.
                    typecheck_expression(arr_deref->array_or_pointer_expression);
                    if (compiler->errors_reported) return;

                    auto left_type = get_type_info(arr_deref->array_or_pointer_expression);
                    if (is_struct_type(left_type)) {
                        auto struct_decl = left_type->struct_decl;

                        collect_function_overloads_for_atom(operator_atom, &struct_decl->member_scope, &ident->overload_set);
                    }

                    if (!ident->overload_set.count) collect_function_overloads_for_atom(operator_atom, bin->enclosing_scope, &ident->overload_set);

                    if (ident->overload_set.count) {
                        Ast_Function_Call *call = SEMA_NEW(Ast_Function_Call);
                        copy_location_info(call, bin);
                        call->argument_list.add(arr_deref->array_or_pointer_expression);
                        call->argument_list.add(arr_deref->index_expression);
                        call->argument_list.add(bin->right);

                        Ast_Function *function = get_best_overload_from_set(call, ident->overload_set);
                        if (compiler->errors_reported) return;

                        if (!function && bin->operator_type == Token::EQUALS) {
                            call->argument_list[0] = make_unary(compiler, Token::STAR, bin->left);
                            copy_location_info(call->argument_list[0], bin->left);
                            function = get_best_overload_from_set(call, ident->overload_set);
                            if (compiler->errors_reported) return;
                        }

                        if (function) {
                            bin->substitution = call;
                            call->function_or_function_ptr = function;
                            typecheck_expression(call);
                            return;
                        }
                    }
                }
            }

            u32 allow_coerce_to_ptr_void_flag = ((bin->operator_type == Token::EQUALS) ? ALLOW_COERCE_TO_PTR_VOID : 0);

            if (bin->operator_type == Token::EQUALS) {
                // we're only allowed to cast on the rhs of an assignment.

                typecheck_expression(bin->left);
                if (compiler->errors_reported) return;

                auto tuple = typecheck_and_implicit_cast_single_expression(bin->right, get_type_info(bin->left), allow_coerce_to_ptr_void_flag);

                bin->right = tuple.item2;
            } else {
                // Special case for enum type comparision with inference.
                // IC: I think this should work with all operators.
                //if (bin->operator_type == Token::EQ_OP || bin->operator_type == Token::NE_OP ||
                //    bin->operator_type == Token::LE_OP || bin->operator_type == Token::GE_OP ||
                //    bin->operator_type == '<' || bin->operator_type == '>' || bin->operator_type == Token::VERTICAL_BAR)
                {
                    if (expression_needs_enum_type_inference(bin->right)) {
                        // If lhs is enum, supply enum type to rhs.
                        typecheck_expression(bin->left);
                        if (bin->left->type_info != nullptr && bin->left->type_info->type == Ast_Type_Info::ENUM) {
                            typecheck_expression(bin->right, bin->left->type_info);
                        }
                    }
                    else if (expression_needs_enum_type_inference(bin->left)) {
                        // If rhs is enum, supply enum type to lhs.
                        typecheck_expression(bin->right);
                        if (bin->right->type_info != nullptr && bin->right->type_info->type == Ast_Type_Info::ENUM) {
                            typecheck_expression(bin->left, bin->right->type_info);
                        }
                    }
                }

                // @Incomplete change typecheck_and_implicit_cast_expression_pair to use flags
                bool allow_coerce_to_ptr_void = (allow_coerce_to_ptr_void_flag & ALLOW_COERCE_TO_PTR_VOID);
                typecheck_and_implicit_cast_expression_pair(bin->left, bin->right, &bin->left, &bin->right, allow_coerce_to_ptr_void);
            }

            if (compiler->errors_reported) return;

            // @Hack @Incomplete
            bin->type_info = get_type_info(bin->left);

            assert(bin->type_info);

            if (is_valid_overloadable_operator(bin->operator_type)) {
                Atom *operator_atom = compiler->make_operator_atom(bin->operator_type);

                Ast_Identifier *ident = SEMA_NEW(Ast_Identifier);
                ident->name = operator_atom;

                // First we check in the type on the left if an overload is available.
                // This should save a lot of time compared to doing a full scope tree lookup.
                auto left_type = get_type_info(bin->left);
                if (is_struct_type(left_type)) {
                    auto struct_decl = left_type->struct_decl;

                    collect_function_overloads_for_atom(operator_atom, &struct_decl->member_scope, &ident->overload_set);
                }

                if (!ident->overload_set.count) collect_function_overloads_for_atom(operator_atom, bin->enclosing_scope, &ident->overload_set);

                if (ident->overload_set.count) {
                    Ast_Function_Call *call = SEMA_NEW(Ast_Function_Call);
                    copy_location_info(call, bin);
                    call->argument_list.add(bin->left);
                    call->argument_list.add(bin->right);

                    Ast_Function *function = get_best_overload_from_set(call, ident->overload_set);
                    if (compiler->errors_reported) return;

                    if (!function && bin->operator_type == Token::EQUALS) {
                        call->argument_list[0] = make_unary(compiler, Token::STAR, bin->left);
                        copy_location_info(call->argument_list[0], bin->left);
                        function = get_best_overload_from_set(call, ident->overload_set);
                        if (compiler->errors_reported) return;
                    }

                    if (function) {
                        bin->substitution = call;
                        call->function_or_function_ptr = function;
                        typecheck_expression(call);
                        return;
                    }
                }
            }

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

            auto left_type  = get_final_type(get_type_info(bin->left));
            auto right_type = get_final_type(get_type_info(bin->right));

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
                    is_pointer_type(left_type) && is_int_type(right_type)) {
                    return;
                }

                // @TODO report operator
                auto lhs = type_to_string(left_type);
                auto rhs = type_to_string(right_type);
                compiler->report_error(bin, "Incompatible types found on lhs and rhs of binary operator (%.*s, %.*s).", lhs.length, lhs.data, rhs.length, rhs.data);
                free(lhs.data);
                free(rhs.data);
                return;
            }

            // IC: I think it's more clear to check the operators for each type independently.
            if (is_enum_type(left_type)) {
                if (bin->operator_type == Token::SLASH ||
                    bin->operator_type == Token::STAR || 
                    bin->operator_type == Token::DEREFERENCE_OR_SHIFT ||
                    bin->operator_type == Token::RIGHT_SHIFT) 
                {
                    // From parser.cpp
                    extern String token_type_to_string(Token::Type type);

                    // @@ Can we get a string that we don't have to deallocate?
                    auto token_string = token_type_to_string(bin->operator_type);
                    defer { free(token_string.data); };

                    auto type_name = type_to_string(left_type);
                    defer { free(type_name.data); };
                    
                    compiler->report_error(bin, "Operator '%s' not valid on '%.*s' type.\n", token_string, type_name);
                    return;
                }
            }
            else {
                if (bin->operator_type == Token::LE_OP ||
                    bin->operator_type == Token::GE_OP ||
                    bin->operator_type == Token::RIGHT_ANGLE ||
                    bin->operator_type == Token::LEFT_ANGLE) {
                    if (!is_int_type(left_type) && !is_float_type(left_type) && !is_enum_type(left_type)) {
                        // @@ Reverse error message? Comparison op not valid for this type.
                        compiler->report_error(bin, "Comparison operators are only valid for integer, floating-point, and enum operands.\n");
                        return;
                    }
                }

                if (bin->operator_type == Token::EQ_OP || bin->operator_type == Token::NE_OP) {
                    if (!is_int_type(left_type) && !is_float_type(left_type) && !is_enum_type(left_type)
                        && left_type->type != Ast_Type_Info::STRING && !is_pointer_type(left_type)
                        && left_type->type != Ast_Type_Info::BOOL && left_type->type != Ast_Type_Info::TYPE
                        && left_type->type != Ast_Type_Info::FUNCTION) {
                        String given = type_to_string(left_type);
                        compiler->report_error(bin, "Equal operator is only valid for integer, floating-point, pointer, string, function, and Type operands (Given %.*s).\n", PRINT_ARG(given));
                        free(given.data);
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
                if (!is_pointer_type(type)) {
                    compiler->report_error(un, "Cannot use '<<' on a non-pointer expression.\n");
                    return;
                }

                un->type_info = type->pointer_to;
            } else if (un->operator_type == Token::MINUS) {
                auto type = get_type_info(un->expression);
                if (!is_int_type(type) && !is_float_type(type)) {
                    compiler->report_error(un, "Unary '-' is only valid for integer for float operands.\n");
                    return;
                }

                // @Incomplete I think, should we warn about unary minus on unsiged integers?
                un->type_info = type;
            } else if (un->operator_type == Token::EXCLAMATION) {
                auto result = typecheck_and_implicit_cast_single_expression(un->expression, compiler->type_bool, ALLOW_COERCE_TO_BOOL);
                auto expr = result.item2;

                if (compiler->errors_reported) return;

                if (get_type_info(expr)->type != Ast_Type_Info::BOOL) {
                    compiler->report_error(expr, "Operand of unary '!' does not coerce to bool.\n");
                    return;
                }

                // @Cleanup use substitution if possible.
                un->expression = expr;
                un->type_info = compiler->type_bool;
            } else if (un->operator_type == Token::TILDE) {
                auto type = get_type_info(un->expression);
                if (!is_int_type(type)) {
                    compiler->report_error(un, "Unary '~' is only valid for integer operands.\n");
                    return;
                }

                un->type_info = type;
            }

            assert(un->type_info);
            return;
        }

        case AST_LITERAL: {
            auto lit = static_cast<Ast_Literal *>(expression);

            // @Incomplete if we have a float literal but want an int type, keep a float type and let the implicit cast system do its job
            if (lit->literal_type == Ast_Literal::INTEGER) {
                // This does not take into account if the literal is negative... but the lexer currently does not
                // lex negative numbers so maybe we shouldnt worry about that here.. @TODO -josh 22 December 2019
                if (static_cast<u32>(lit->integer_value) == lit->integer_value)
                    lit->type_info = compiler->type_int32;
                else
                    lit->type_info = compiler->type_int64;
            }

            if (lit->literal_type == Ast_Literal::FLOAT) {
                lit->type_info = compiler->type_float64; // @TODO we should probably have a check that verifies if the literal can fit in a 32-bit float and then default to that.
            }

            if (lit->literal_type == Ast_Literal::STRING)  lit->type_info = compiler->type_string;

            if (lit->literal_type == Ast_Literal::BOOL) lit->type_info = compiler->type_bool;

            if (lit->literal_type == Ast_Literal::NULLPTR) {
                lit->type_info = compiler->type_ptr_void;
            }

            assert(lit->type_info);

            return;
        }

        case AST_FUNCTION: {
            auto function = static_cast<Ast_Function *>(expression);
            if (do_function_body)
                typecheck_function(function);
            else
                typecheck_function_header(function);
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
                    auto left_type = get_type_info(deref->original_left);
                    bool is_struct = (is_pointer_type(left_type) && is_struct_type(left_type->pointer_to))
                                    || is_struct_type(left_type);
                    bool is_array  = (is_pointer_type(left_type) && is_array_type(left_type->pointer_to))
                                    || is_array_type(left_type);
                    if (is_struct || is_array) {
                        implicit_argument = deref->original_left;
                    }
                }
            }

            if (implicit_argument) {
                call->argument_list.insert(0, implicit_argument);

                // function_call_is_viable will check for this to cast the implicit argument to the target type if able.
                call->implicit_argument_inserted = true;
            }

            while (subexpression->substitution) subexpression = subexpression->substitution;

            if (subexpression->type == AST_IDENTIFIER) {
                auto identifier = static_cast<Ast_Identifier *>(subexpression);
                auto overload_set = identifier->overload_set;

                // If overload_set is empty, then it may still be a function pointer.
                if (overload_set.count) {
                    Ast_Function *function = get_best_overload_from_set(call, overload_set);
                    if (compiler->errors_reported) return;

                    if (!function) {
                        compiler->report_error(call, "No viable overload for function call.\n");

                        auto errors_reported = compiler->errors_reported;

                        for (auto func : overload_set) {
                            // @TODO this is a bit of an easy out for the time being, there are many ways to improve this with better diagnostics.
                            compiler->errors_reported = 0; // Clear errors_reported so that we can get the full diagnostics from the call checking

                            if (func->is_template_function) {
                                get_polymorph_for_function_call(func, call, true);
                                continue;
                            }
                            function_call_is_viable(call, get_type_info(func), func, true);
                        }

                        compiler->errors_reported = errors_reported;
                        return;
                    }

                    function_call_is_viable(call, get_type_info(function), function, true);
                    if (compiler->errors_reported) return;

                    if (function->return_type) {
                        call->type_info = function->return_type->type_value;
                    } else {
                        call->type_info = compiler->type_void;
                    }

                    identifier->resolved_declaration = function;
                    identifier->type_info = function->type_info;
                    return;
                }
            }

            // fall through case for expressions that generate function pointers
            auto info = get_final_type(get_type_info(subexpression));

            if (info->type != Ast_Type_Info::FUNCTION) {
                String given = type_to_string(info);
                compiler->report_error(call, "Function-call target does not name a function (Given %.*s).\n", PRINT_ARG(given));
                free(given.data);
                return;
            }

            auto tuple = function_call_is_viable(call, info, nullptr, true);

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

            Ast_Type_Info * left_type = nullptr;

            if (deref->left == nullptr) {
                // Try to infer dereferenced type.
                left_type = want_numeric_type;

                if (!left_type || left_type->type != Ast_Type_Info::ENUM) {
                    compiler->report_error(deref, "Cannot infer enum type for expression.\n");
                    return;
                }

                deref->is_type_dereference = true;
            }
            else {
                typecheck_expression(deref->left);
                if (compiler->errors_reported) return;
            
                // Save this in case we need to check what the original thing was for struct member-function overloading.
                // We dont want this change to propagate down to function-overloading.
                deref->original_left = deref->left;

                left_type = get_type_info(deref->left);
                assert(left_type);

                auto left = deref->left;
                if (left_type->type == Ast_Type_Info::TYPE) {
                    deref->is_type_dereference = true;
                    while (left->substitution) left = left->substitution;

                    if (left->type == AST_IDENTIFIER) {
                        auto identifier = static_cast<Ast_Identifier *>(left);

                        left = identifier->resolved_declaration;
                        left_type = get_type_info(left);
                    }

                    if (left->type == AST_TYPE_ALIAS) {
                        auto alias = static_cast<Ast_Type_Alias *>(left);

                        left_type = alias->type_value;
                        left_type = get_final_type(left_type);

                        assert(left_type);
                        assert(alias->type_value->type == Ast_Type_Info::ALIAS);
                    } else if (left->type == AST_STRUCT) {
                        auto _struct = static_cast<Ast_Struct *>(left);

                        left_type = _struct->type_value;
                        assert(left_type);
                    }
                    else if (left->type == AST_ENUM) {
                        auto _enum = static_cast<Ast_Enum *>(left);

                        left_type = _enum->type_value;
                        assert(left_type);
                    }
                }

                if (is_pointer_type(get_type_info(deref->left))) {
                    // we allow you to dereference once through a pointer

                    left_type = get_final_type(get_type_info(deref->left))->pointer_to;
                }

                left_type = get_final_type(left_type);
                if (left_type->type != Ast_Type_Info::STRING &&
                    left_type->type != Ast_Type_Info::ARRAY  &&
                    left_type->type != Ast_Type_Info::STRUCT &&
                    left_type->type != Ast_Type_Info::ENUM) {
                    String given = type_to_string(left_type);
                    compiler->report_error(deref, "Attempt to dereference a type that is not a string, struct, array, or enum! (Given %.*s)\n", PRINT_ARG(given));
                    free(given.data);
                    return;
                }
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
                        String func_name = mprintf("__array_%.*s", PRINT_ARG(field_atom->name));
                        auto ident = make_identifier(compiler, compiler->make_atom(func_name));
                        free(func_name.data);

                        ident->enclosing_scope = deref->field_selector->enclosing_scope;
                        copy_location_info(ident, deref);

                        typecheck_expression(ident, want_numeric_type, overload_set_allowed);
                        if (compiler->errors_reported) return;

                        deref->substitution = ident;
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
                        // @TODO should we allow <array>.func() => __array_func() syntax for known-size arrays?
                        String field_name = field_atom->name;
                        compiler->report_error(deref, "No member '%.*s' in known-size array.\n", field_name.length, field_name.data);
                    }
                }
            } else if (left_type->type == Ast_Type_Info::STRUCT) {
                // @Hack if clang_import has imported a struct and jiyu code uses a field of that struct
                // sometimes the struct has not be typechecked at this point, but we have an Ast_Type_Info node,
                // so attempt to typecheck it first. We probably want to do something more robust in the future in
                // terms of AST nodes generated by clang_import. -josh 29 December 2019
                typecheck_expression(left_type->struct_decl);
                if (compiler->errors_reported) return;

                // @Incomplete this should perform a scope lookup for a declaration so we can handle
                // lets, functions, typealiases, etc..
                bool found = check_type_info_for_struct_member(compiler, deref, left_type, field_atom);

                if (found && deref->is_type_dereference) {
                    compiler->report_error(deref, "Attempt to use struct variable member without an instance!\n");
                    return;
                }

                if (!found) {
                    auto _struct = left_type->struct_decl;
                    assert(_struct);

                    auto decl = find_declaration_for_atom_in_scope(&_struct->member_scope, field_atom);
                    Ast_Scope *decl_scope = &_struct->member_scope;

                    // @Incomplete this doesnt check the parent_struct's potential parent_struct.
                    if (!decl && _struct->parent_struct) {
                        auto parent = _struct->parent_struct->type_value->struct_decl;

                        assert(parent && parent->type == AST_STRUCT);
                        decl = find_declaration_for_atom_in_scope(&parent->member_scope, field_atom);
                        decl_scope = &parent->member_scope;
                    }

                    if (decl && decl->type == AST_DECLARATION) {
                        // this is not supposed to happen because regular var's should be handled by the above code.
                        if (deref->is_type_dereference) assert(static_cast<Ast_Declaration *>(decl)->is_let);
                    } else if (decl && decl->type == AST_FUNCTION) {
                        // @Hack substitute to an identifier to get the quick benefit of the overloading sytem.

                        auto ident = make_identifier(compiler, field_atom);
                        ident->enclosing_scope = decl_scope;

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
                            decl = ident;
                        } else {
                            // resolved_declaration and type_info will be resolved by the Ast_Function_Call code.
                            // Set to void for now, Ast_Function_Call code will either error or fix this up.
                            ident->type_info = compiler->type_void;

                            decl = ident;
                        }
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

                    if (!left_type->is_tuple) {
                        const char *TYPE = "struct";
                        if (left_type->is_union) TYPE = "union";
                        String name = left_type->struct_decl->identifier->name->name;
                        compiler->report_error(deref, "No member '%.*s' in %s %.*s.\n", PRINT_ARG(field_name), TYPE, PRINT_ARG(name));
                    } else {
                        String tuple_type = type_to_string(left_type);
                        compiler->report_error(deref, "No member '%.*s' in tuple %.*s.\n", PRINT_ARG(field_name), PRINT_ARG(tuple_type));
                    }
                }
            } else if (left_type->type == Ast_Type_Info::ENUM) {
                auto _enum = left_type->enum_decl;

                bool found = false;
                for (auto member : _enum->member_scope.declarations) {
                    auto decl = static_cast<Ast_Declaration *>(member);
                    if (decl->identifier->name == field_atom) {
                        assert(decl->is_let);

                        typecheck_expression(decl);
                        if (compiler->errors_reported) return;

                        // Substitute Enum.Value by Value's declaration.
                        deref->substitution = decl;
                        deref->type_info = decl->type_info;
                        found = true;
                        break;
                    }
                }

                if (!found) {
                    String field_name = field_atom->name;
                    String name = left_type->enum_decl->identifier->name->name;
                    compiler->report_error(deref, "No member '%.*s' in enum %.*s.\n", PRINT_ARG(field_name), PRINT_ARG(name));
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
            if (get_final_type(get_type_info(cond))->type != Ast_Type_Info::BOOL) {
                String given = type_to_string(get_final_type(get_type_info(cond)));
                compiler->report_error(cond, "'if' condition isn't of boolean type (Given %.*s).\n", PRINT_ARG(given));
                free(given.data);
            }

            typecheck_scope(&_if->then_scope);
            if (_if->else_scope) typecheck_scope(_if->else_scope);

            return;
        }

        case AST_WHILE: {
            auto loop = static_cast<Ast_While *>(expression);

            auto result = typecheck_and_implicit_cast_single_expression(loop->condition, compiler->type_bool, ALLOW_COERCE_TO_BOOL);
            loop->condition = result.item2;

            if (compiler->errors_reported) return;

            auto cond = loop->condition;
            if (get_type_info(cond)->type != Ast_Type_Info::BOOL) {
                compiler->report_error(cond, "'while' condition isn't of boolean type.\n");
            }

            typecheck_scope(&loop->body);

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

            auto init_type = get_type_info(_for->initial_iterator_expression);
            if (!_for->upper_range_expression) {
                if (is_int_type(init_type)) {
                    compiler->report_error(_for, "'for' must specify an upper-range. Ex: for 0..1\n");
                    return;
                }
            } else {
                if (!is_int_type(init_type)) {
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
                if (!is_int_type(upper_type)) {
                    String given = type_to_string(upper_type);
                    compiler->report_error(_for->upper_range_expression, "'for' upper-range must be an integer expression (Given %.*s).\n", PRINT_ARG(given));
                    free(given.data);
                    return;
                }

                if (!types_match(init_type, upper_type)) {
                    compiler->report_error(_for, "'for' lower-range and upper-range types do not match!\n");
                }
            }

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
                
                Ast_Identifier * it_index_ident = nullptr;
              
                {
                    Ast_Declaration *decl = SEMA_NEW(Ast_Declaration);
                    if (_for->iterator_index_ident) {
                        it_index_ident = decl->identifier = _for->iterator_index_ident;
                        copy_location_info(decl, _for->iterator_index_ident);
                    }
                    else {
                        copy_location_info(decl, _for);
                        it_index_ident = decl->identifier = make_identifier(compiler, compiler->atom_it_index);
                        copy_location_info(decl->identifier, _for);
                    }
                    it_index_ident->enclosing_scope = &_for->body;
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
                    if (_for->iterator_ident) {
                        decl->identifier = _for->iterator_ident;
                        copy_location_info(decl, _for->iterator_ident);
                    }
                    else {
                        copy_location_info(decl, _for);
                        decl->identifier = make_identifier(compiler, compiler->atom_it);
                        copy_location_info(decl->identifier, decl);
                    }
                    decl->identifier->enclosing_scope = &_for->body;
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
                if (_for->iterator_ident) {
                    decl->identifier = _for->iterator_ident;
                    copy_location_info(decl, _for->iterator_ident);
                }
                else {
                    copy_location_info(decl, _for);
                    decl->identifier = make_identifier(compiler, compiler->atom_it);
                    copy_location_info(decl->identifier, _for);
                }
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
            if (function->return_type) {
                // since we are currently in the scope of this function, return_decl should be gauranteed to be typechecked already.
                assert(get_type_info(function->return_type));

                auto return_type = function->return_type->type_value;

                if (!ret->expression) {
                    String name = type_to_string(return_type);
                    compiler->report_error(ret, "'return' statement must return an expression of function return type %.*s.\n", name.length, name.data);
                    return;
                }

                auto result = typecheck_and_implicit_cast_single_expression(ret->expression, return_type, ALLOW_COERCE_TO_PTR_VOID);
                if (compiler->errors_reported) return;

                if (result.item2 != ret->expression) ret->expression = result.item2; // @Cleanup Should be using substitutions

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
                if (compiler->errors_reported) return;
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
                compiler->report_error(cast, "Invalid cast.\n");
                return;
            }

            return;
        }

        case AST_TYPE_ALIAS: {
            auto alias = static_cast<Ast_Type_Alias *>(expression);

            if (alias->internal_type_inst) {
                resolve_type_inst(alias->internal_type_inst);
                if (compiler->errors_reported) return;

                // internal_type_inst->type_value can be unfilled if this is a reference to a polymorphic struct
                if (alias->internal_type_inst->type_value) {
                    alias->type_value = compiler->make_type_alias_type(alias->internal_type_inst->type_value);
                    alias->type_value->alias_decl = alias;
                }
            } else {
                // We got here due to polymorphing taking advantage of the
                // alias system. No need to create an _alias_ type, but maybe
                // we should for error reporting clarity.
                assert(alias->type_value);

                // In the case that we got here due to clang_import, typecheck struct_decl if able
                // Otherwise something might try to resolve before struct_decl is fully typechecked.
                if (get_final_type(alias->type_value)->struct_decl) typecheck_expression(get_final_type(alias->type_value)->struct_decl);
            }

            alias->type_value->is_distinct = alias->is_distinct;
            alias->type_info = compiler->type_info_type;
            return;
        }

        case AST_STRUCT: {
            auto _struct = static_cast<Ast_Struct *>(expression);

            if (_struct->is_anonymous) {
                char *typekind = "struct";
                if (_struct->is_union) typekind = "union";

                if (_struct->is_template_struct) {
                    compiler->report_error(_struct, "Anonymous %s cannot be a template.\n", typekind);
                    return;
                }

                auto owner = get_nearest_owner(_struct->member_scope.parent);
                if (!owner || (owner && owner->type != AST_STRUCT)) {
                    compiler->report_error(_struct, "Anonymous %s may only be declared within another struct or union.\n", typekind);
                    return;
                }

                if (_struct->parent_struct) {
                    compiler->report_error(_struct, "Anonymous %s cannot inherit from a parent type.\n", typekind);
                }
            }

            if (_struct->is_template_struct) {
                // Legal use of a template struct is only from a polymorph of this struct.
                // So the plain version of the struct is void.
                _struct->type_info = compiler->type_void;
                return;
            }

             if (!_struct->type_info) {

                // Set this early so we dont recurse indefinitely
                if (!_struct->type_value) {
                    // If this is already set, this may have been due to a clang_import
                    _struct->type_value = make_struct_type(compiler, _struct);
                } else {
                    assert(_struct->type_value->struct_members.count == 0);
                    assert(_struct->type_value->type_table_index == -1);
                }

                _struct->type_info = compiler->type_info_type;

                if (_struct->parent_struct) {
                    if (_struct->is_union) {
                        compiler->report_error(_struct, "Unions may not inherit from other types.\n");
                        return;
                    }
                    typecheck_expression(_struct->parent_struct);
                    if (compiler->errors_reported) return;

                    // @Incomplete we should probably make this work with typealiases.
                    if (!is_struct_type(_struct->parent_struct->type_value)) {
                        compiler->report_error(_struct->parent_struct, "Struct parent must be a struct.\n");
                        return;
                    }

                    _struct->type_value->parent_struct = _struct->parent_struct->type_value;
                }

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

                    s64 size = 0;
                    s64 offset_cursor = 0;
                    s64 biggest_alignment = 1;
                    s64 element_path_index = 0;

                    if (_struct->parent_struct) {
                        auto type_value = _struct->parent_struct->type_value;
                        element_path_index = get_final_type(type_value)->struct_decl->final_element_path_index;
                        assert(element_path_index > 0);

                        // Since the parent struct is used as if the individual fields have been declared within this struct
                        // we use the size instead of the stride.
                        offset_cursor = type_value->size;
                    }

                    // This is likely super @Incomplete                
                    for (auto expr : _struct->member_scope.declarations) {
                        if (expr->type == AST_DECLARATION) {

                            // @Cleanup @Hack we need to be able to handle other structs, functions, typealiases or at least punt on them.
                            auto decl = static_cast<Ast_Declaration *>(expr);
                            typecheck_expression(decl);
                            if (compiler->errors_reported) return;

                            if (decl->is_let) continue;

                            assert(decl && decl->type_info);

                            Ast_Type_Info::Struct_Member member;
                            member.name = decl->identifier->name;
                            member.type_info = decl->type_info;

                            member.element_index = element_path_index;
                            element_path_index++;

                            auto final_type = get_final_type(member.type_info);
                            if (final_type->size == -1) {
                                auto member_type_name = type_to_string(member.type_info);
                                defer { free(member_type_name.data); };
                                compiler->report_error(decl, "field '%.*s' has incomplete type '%.*s'\n", PRINT_ARG(member.name->name), PRINT_ARG(member_type_name));
                                // @@ Definition of '%s' is not complete until the closing '}'
                                return;
                            }

                            auto alignment = final_type->alignment;
                            if (info->alignment >= 1 && info->alignment < final_type->alignment) alignment = info->alignment;

                            String name;
                            if (member.name) name = member.name->name;
                            offset_cursor = pad_to_alignment(offset_cursor, alignment);
                            member.offset_in_struct = offset_cursor;

                            assert(final_type->stride >= 0);
                            if (!_struct->is_union) {
                                offset_cursor += final_type->stride;
                                size = offset_cursor;
                            } else {
                                if (final_type->stride > size) {
                                    size = final_type->stride;
                                }
                            }

                            if (alignment > biggest_alignment) {
                                biggest_alignment = alignment;
                            }

                            info->struct_members.add(member);
                        } else if (expr->type == AST_STRUCT) {
                            auto _substruct = static_cast<Ast_Struct *>(expr);
                            if (_substruct->is_anonymous) {
                                typecheck_expression(_substruct);
                                if (compiler->errors_reported) return;

                                auto subtype = get_type_declaration_resolved_type(_substruct);

                                auto final_type = subtype;

                                Ast_Type_Info::Struct_Member member;
                                member.name = nullptr;
                                member.type_info = final_type;
                                member.is_anonymous_struct = true;
                                
                                member.element_index = element_path_index;
                                element_path_index++;

                                if (final_type->size == -1) {
                                    char *typekind = "struct";
                                    if (_substruct->is_union) typekind = "union";
                                    compiler->report_error(_substruct, "anonymous %s is an incomplete type.\n", typekind);
                                    // @@ Definition of '%s' is not complete until the closing '}'
                                    return;
                                }

                                auto alignment = final_type->alignment;
                                if (info->alignment >= 1 && info->alignment < final_type->alignment) alignment = info->alignment;

                                offset_cursor = pad_to_alignment(offset_cursor, alignment);
                                member.offset_in_struct = offset_cursor;

                                assert(final_type->stride >= 0);
                                if (!_struct->is_union) {
                                    offset_cursor += final_type->stride;
                                    size = offset_cursor;
                                } else {
                                    if (final_type->stride > size) {
                                        size = final_type->stride;
                                    }
                                }

                                if (alignment > biggest_alignment) {
                                    biggest_alignment = alignment;
                                }

                                info->struct_members.add(member);
                            }
                        }
                    }

                    if (info->alignment <= 0) info->alignment = biggest_alignment;

                    if (info->size >= 0) assert(pad_to_alignment(size, info->alignment) == info->size); //this came from clang
                    info->size = size;
                    info->stride = pad_to_alignment(info->size, info->alignment);

                    _struct->final_element_path_index = element_path_index;

                    compiler->add_to_type_table(info);
                }
            }

            if (only_want_struct_type) return;

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

            auto result = typecheck_and_implicit_cast_single_expression(deref->index_expression, compiler->type_array_count, 0);
            if (result.item2 != deref->index_expression) deref->index_expression = result.item2; // @Cleanup use substitution if available

            if (compiler->errors_reported) return;

            if (!types_match(get_type_info(deref->index_expression), compiler->type_array_count)) {
                // If the index expression is unsigned and doesnt implicitly case to type_array_count,
                // Attempt to cast to the unsigned version of type_array_count so that LLVM does not
                // try to implicitly sign-extend a uint16 to int32

                auto result = typecheck_and_implicit_cast_single_expression(deref->index_expression, compiler->type_array_count_unsigned, 0);
                if (result.item2 != deref->index_expression) deref->index_expression = result.item2; // @Cleanup use substitution if available
            }

            {
                // Mostly @Cutnpaste from the Ast_Binary_Expression stuff
                Atom *operator_atom = compiler->make_atom(OPERATOR_BRACKET_NAME);

                Ast_Identifier *ident = SEMA_NEW(Ast_Identifier);
                copy_location_info(ident, deref);
                ident->name = operator_atom;
                collect_function_overloads_for_atom(operator_atom, deref->enclosing_scope, &ident->overload_set);

                if (ident->overload_set.count) {
                    Ast_Function_Call *call = SEMA_NEW(Ast_Function_Call);
                    copy_location_info(call, deref);
                    call->argument_list.add(deref->array_or_pointer_expression);
                    call->argument_list.add(deref->index_expression);

                    Ast_Function *function = get_best_overload_from_set(call, ident->overload_set);
                    if (compiler->errors_reported) return;

                    if (function) {
                        deref->substitution = call;
                        call->function_or_function_ptr = function;
                        typecheck_expression(call);
                        return;
                    }
                }
            }

            auto array_type = get_type_info(deref->array_or_pointer_expression);
            array_type = get_final_type(array_type);

            if (array_type->type != Ast_Type_Info::ARRAY   && array_type->type != Ast_Type_Info::POINTER &&
                array_type->type != Ast_Type_Info::STRING) {
                compiler->report_error(deref->array_or_pointer_expression, "Expected array, string, or pointer for index expression, but got something else.\n");
                return;
            }

            auto index_type = get_type_info(deref->index_expression);
            if (!is_int_type(index_type)) {
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
            if (compiler->errors_reported) return;

            auto final_type = get_final_type(type);

            Ast_Literal *lit = nullptr;
            if (size->operator_type == Token::KEYWORD_SIZEOF) {
                lit = make_integer_literal(compiler, final_type->size, compiler->type_int32);
            } else if (size->operator_type == Token::KEYWORD_STRIDEOF) {
                lit = make_integer_literal(compiler, final_type->stride, compiler->type_int32);
            } else if (size->operator_type == Token::KEYWORD_ALIGNOF) {
                lit = make_integer_literal(compiler, final_type->alignment, compiler->type_int32);
            }
            assert(lit);

            copy_location_info(lit, size);
            size->type_info = lit->type_info;
            size->substitution = lit;
            return;
        }
        case AST_TYPEOF: {
            Ast_Typeof *typeof = static_cast<Ast_Typeof *>(expression);

            if (!typeof->expression) {
                compiler->report_error(typeof, "typeof() must take one argument.\n");
                return;
            }

            typecheck_expression(typeof->expression);
            if (compiler->errors_reported) return;

            auto expr_type = get_type_info(typeof->expression);

            Ast_Type_Instantiation *type_inst = SEMA_NEW(Ast_Type_Instantiation);
            copy_location_info(type_inst, typeof);
            type_inst->type_value = expr_type;
            type_inst->type_info = compiler->type_info_type;

            typeof->substitution = type_inst;
            typeof->type_info = compiler->type_info_type;
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

            auto os_type = compiler->llvm_gen->TargetMachine->getTargetTriple().getOS();
            auto os_name = llvm::Triple::getOSTypeName(os_type);

            auto ident_string = copy_string(ident->name->name);
            // @FixMe be weary of a UTF8 string here.
            for (string_length_type i = 0; i < ident_string.length; ++i) {
                ident_string.data[i] = tolower(ident_string.data[i]);
            }

            lit->bool_value = (ident_string == to_string(os_name.str().c_str()));
            // printf("%s, os(%.*s): %d\n", os_name.str().c_str(), PRINT_ARG(ident_string), lit->bool_value);

            if (ident_string == to_string("macosx") && os_type == llvm::Triple::Darwin) {
                // Give the programmer a helping hand here since apple-darwin and apple-macosx seem to be the same thing.
                // apple-darwin is more common. It seems iOS, tvOS, watchOS all contain their OS-names in their triple.
                // @TODO we may want to do something similar if the programmer specifies os(Darwin)... we want it to be
                // true for all these Darwin-derived OS's. -josh 24 December 2019
                lit->bool_value = true;
            }

            free(ident_string.data);

            // @TODO add an error if the input to os() is not a valid, maybe.
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

        case AST_ENUM: {
            auto _enum = static_cast<Ast_Enum *>(expression);
            
            // Set this early so we dont recurse indefinitely
            _enum->type_value = compiler->make_enum_type(_enum);
            _enum->type_info = compiler->type_info_type;
            
            auto base_type_info = resolve_type_inst(_enum->base_type);
            if (compiler->errors_reported) return;

            _enum->type_value->size = base_type_info->size;
            _enum->type_value->alignment = base_type_info->alignment;
            _enum->type_value->stride = base_type_info->stride;
            _enum->type_value->is_signed = base_type_info->is_signed;

            // flag stuct member declarations
            for (auto _decl : _enum->member_scope.declarations) {
                assert (_decl->type == AST_DECLARATION);
                auto decl = static_cast<Ast_Declaration *>(_decl);
                decl->is_enum_member = true;
            }
            
            {
                auto info = _enum->type_value;
                assert(info->type == Ast_Type_Info::ENUM);
                assert(info->enum_decl == _enum);

                info->enum_base_type = base_type_info;
                                
                // Set to -1 so that first item is initialized to 0.
                bool prev_value_set = false;
                s64 prev_value;

                for (auto expr : _enum->member_scope.declarations) {
                    assert (expr->type == AST_DECLARATION);
                    auto decl = static_cast<Ast_Declaration *>(expr);
                    assert(decl->is_let);

                    Ast_Literal * literal = nullptr;
                    if (!decl->initializer_expression) {
                        s64 value;
                        if (_enum->is_flags) {
                            value = 1;
                            if (prev_value_set) {
                                // Make sure prev_value only has one bit set.
                                if (!is_pow2(prev_value)) {
                                    compiler->report_error(decl, "Implicit flag initialization not allowed after elements with more than one bit set.");
                                    return;
                                }

                                value = prev_value * 2;

                                if ((u64)value > max_integer(base_type_info->size, base_type_info->is_signed)) {
                                    compiler->report_error(decl, "Implicit flag initialization overflows internal enum type (%.*s == %lld).", PRINT_ARG(decl->identifier->name->name), value);
                                    return;
                                }
                            }
                        }
                        else {
                            value = 0;
                            if (prev_value_set) {
                                if (prev_value == (s64)max_integer(base_type_info->size, base_type_info->is_signed)) {
                                    compiler->report_error(decl, "Implicit enum value initialization overflows internal enum type. (%d == %d)", prev_value, max_integer(base_type_info->size, base_type_info->is_signed));
                                    return;
                                }

                                value = prev_value + 1;
                            }
                        }

                        decl->initializer_expression = make_integer_literal(compiler, value, _enum->type_value, decl); 
                    }
                    
                    literal = folds_to_literal(decl->initializer_expression);

                    typecheck_expression(decl);
                    if (compiler->errors_reported) return;

                    assert(literal && literal->type == AST_LITERAL && literal->literal_type == Ast_Literal::INTEGER);
                    prev_value = literal->integer_value;
                    prev_value_set = true;
                    
                    assert(decl && decl->type_info);
                }

                compiler->add_to_type_table(info);
            }
            
            typecheck_scope(&_enum->member_scope);
            if (compiler->errors_reported) return;

            return;
        }

        case AST_TUPLE_EXPRESSION: {
            Ast_Tuple_Expression *tuple = static_cast<Ast_Tuple_Expression *>(expression);

            if (!want_numeric_type) {
                Ast_Struct *tuple_struct = SEMA_NEW(Ast_Struct);
                copy_location_info(tuple_struct, tuple);

                tuple_struct->is_tuple = true;

                for (array_count_type i = 0; i < tuple->arguments.count; ++i) {
                    auto arg = tuple->arguments[i];
                    typecheck_expression(arg);
                    if (compiler->errors_reported) return;

                    Ast_Declaration *decl = SEMA_NEW(Ast_Declaration);
                    copy_location_info(decl, arg);

                    decl->type_info = get_type_info(arg);

                    String name = mprintf("item%d", i);
                    Atom *atom = compiler->make_atom(name);
                    free(name.data);

                    Ast_Identifier *ident = SEMA_NEW(Ast_Identifier);
                    ident->name = atom;
                    copy_location_info(ident, decl);
                    decl->identifier = ident;

                    tuple_struct->member_scope.declarations.add(decl);
                }

                typecheck_expression(tuple_struct);
                if (compiler->errors_reported) return;

                tuple->type_info = tuple_struct->type_value;
            } else {
                auto final_type = get_final_type(want_numeric_type);
                if (!final_type->is_tuple) {
                    // @FixMe error message coule be more clear.
                    compiler->report_error(tuple, "Could not infer type for tuple expression.\n");
                    return;
                }
                assert(final_type->type == Ast_Type_Info::STRUCT);

                if (final_type->struct_members.count != tuple->arguments.count) {
                    compiler->report_error(tuple, "Mismatch in number of arguments for tuple expression (wanted %d, got %d).\n", final_type->struct_members.count, tuple->arguments.count);
                    return;
                }

                for (array_count_type i = 0; i < tuple->arguments.count; ++i) {
                    auto want_type = final_type->struct_members[i].type_info;
                    auto result = typecheck_and_implicit_cast_single_expression(tuple->arguments[i], want_type, ALLOW_COERCE_TO_BOOL | ALLOW_COERCE_TO_PTR_VOID);
                    if (compiler->errors_reported) return;

                    auto expr = result.item2;
                    auto given_type = get_type_info(expr);
                    if (!types_match(given_type, want_type)) {
                        auto wanted = type_to_string(want_type);
                        auto given  = type_to_string(given_type);
                        compiler->report_error(tuple, "Mismatch in argument types. (Wanted %.*s, Given %.*s).\n",
                                               PRINT_ARG(wanted), PRINT_ARG(given));

                        free(wanted.data);
                        free(given.data);
                        return;
                    }

                    if (expr != tuple->arguments[i]) tuple->arguments[i] = expr; // @Cleanup should be using substituion.
                }

                tuple->type_info = want_numeric_type;
            }
            return;
        }

        case AST_SWITCH: {
            auto _switch = static_cast<Ast_Switch *>(expression);

            typecheck_expression(_switch->condition);
            if (compiler->errors_reported) return;

            if (!is_int_or_enum_type(get_type_info(_switch->condition))) {
                compiler->report_error(_switch->condition, "Only integer and enum types are supported in switch conditions at this time.\n");
                return;
            }

            // set type_info before checking the scope such that we do not recurse indefinitely when checking Ast_Case.
            _switch->type_info = compiler->type_void;

            typecheck_scope(&_switch->scope);

            for (auto stmt: _switch->scope.statements) {
                // @Incomplete this does not account for Ast_Scope_Expansion due to static-if
                if (stmt->type != AST_CASE) {
                    compiler->report_error(stmt, "Only 'case' statements are allowed within the scope of 'switch'.\n");
                    return;
                }

                auto _case = static_cast<Ast_Case *>(stmt);
                _switch->cases.add(_case);
            }

            Array<Tuple<s64, Ast_Expression *>> previous_conditions;

            for (auto c : _switch->cases) {
                for (auto cond: c->conditions) {
                    auto lit = resolves_to_literal_value(cond);
                    assert(lit); // This should be gauranteed by type checking at this point
                    assert(lit->literal_type == Ast_Literal::INTEGER);

                    s64 value = lit->integer_value;

                    bool exists = false;

                    for (auto &prev : previous_conditions) {
                        if (value == prev.item1) {
                            exists = true;
                            compiler->report_error(cond, "Duplicate condition in switch case.\n");

                            compiler->report_error(prev.item2, "Previous condition here.\n");
                        }
                    }

                    if (!exists) previous_conditions.add(MakeTuple<s64, Ast_Expression *>(value, cond));
                }
            }


            return;
        }

        case AST_CASE: {
            auto _case = static_cast<Ast_Case *>(expression);

            auto owner = get_nearest_owner(_case->scope.parent);
            bool owner_is_valid = (owner && owner->type == AST_SWITCH);

            if (!owner_is_valid) {
                compiler->report_error(_case, "'case' statement only allowed within the scope of a 'switch' statement.\n");
                return;
            }

            // set type_info before checking the switch to avoid accidentally typechecking this case twice.
            _case->type_info = compiler->type_void;

            auto _switch = static_cast<Ast_Switch *>(owner);
            typecheck_expression(_switch);
            if (compiler->errors_reported) return;

            _case->target_switch = _switch;

            auto cond_type = get_type_info(_switch->condition);

            for (array_count_type i = 0; i < _case->conditions.count; ++i) {
                auto result = typecheck_and_implicit_cast_single_expression(_case->conditions[i], cond_type, 0);

                if (result.item2 != _case->conditions[i]) _case->conditions[i] = result.item2; // @Cleanup use substitution if possible

                if (!folds_to_literal(_case->conditions[i])) {
                    compiler->report_error(_case, "'case' condition must resolve to a literal expression.\n");
                    return;
                }

                auto type = get_type_info(_case->conditions[i]);
                if (!types_match(cond_type, type)) {
                    auto wanted = type_to_string(cond_type);
                    auto given  = type_to_string(type);
                    compiler->report_error(_case->conditions[i], "Mismatch in switch condition types. (Wanted %.*s, Given %.*s).\n",
                                           wanted.length, wanted.data, given.length, given.data);

                    free(wanted.data);
                    free(given.data);
                    return;
                }
            }

            typecheck_scope(&_case->scope);

            return;
        }
    }

    assert(false);
    return;
}

Ast_Struct *Sema::get_polymorph_for_struct(Ast_Struct *_struct, Array<Ast_Type_Instantiation *> &type_args, Ast *site) {
    assert(_struct->is_template_struct);

    if (_struct->polymorphic_type_alias_scope->declarations.count != type_args.count) {
        compiler->report_error(site, "Incorrect number of type arguments for template struct instantiation (Wanted %d, Given %d).\n", _struct->polymorphic_type_alias_scope->declarations.count, type_args.count);
        return nullptr;
    }

    for (auto arg: type_args) {
        typecheck_expression(arg);
        if (compiler->errors_reported) return nullptr;
    }

    for (auto existing: _struct->polymorphed_structs) {
        bool viable = true;
        for (array_count_type i = 0; i < existing->polymorphic_type_alias_scope->declarations.count; ++i) {
            auto src_type = type_args[i]->type_value;
            assert(src_type);

            auto dst = static_cast<Ast_Type_Alias *>(existing->polymorphic_type_alias_scope->declarations[i]);
            assert(dst->type == AST_TYPE_ALIAS);

            auto dst_type = get_type_declaration_resolved_type(dst);

            if (!types_match(src_type, dst_type)) {
                viable = false;
                break;
            }
        }

        if (viable) return existing;
    }

    compiler->copier->scope_stack.add(_struct->polymorphic_type_alias_scope->parent);
    auto copy = static_cast<Ast_Struct *>(compiler->copier->copy(_struct));
    compiler->copier->scope_stack.pop();
    assert(copy && copy->type == AST_STRUCT);

    copy->is_template_struct = false;
    copy->polymorph_source_struct = _struct;
    for (array_count_type i = 0; i < copy->polymorphic_type_alias_scope->declarations.count; ++i) {
        auto alias = static_cast<Ast_Type_Alias *>(copy->polymorphic_type_alias_scope->declarations[i]);
        assert(alias->type == AST_TYPE_ALIAS);

        auto type_value = type_args[i]->type_value;
        assert(type_value);

        alias->type_value = type_value;
    }

    _struct->polymorphed_structs.add(copy);

    typecheck_expression(copy, /*want_numeric_type*/nullptr, /*overload_set_allowed*/false, /*do_function_body*/false, /*only_want_struct_type*/false);
    return copy;
}

Ast_Type_Info *Sema::resolve_type_inst(Ast_Type_Instantiation *type_inst) {
    if (type_inst->type_value) return type_inst->type_value;

    if (type_inst->pointer_to) {
        auto pointee = resolve_type_inst(type_inst->pointer_to);
        if (compiler->errors_reported) return nullptr;

        type_inst->type_value = compiler->make_pointer_type(pointee);
        return type_inst->type_value;
    }

    if (type_inst->builtin_primitive) {
        type_inst->type_value = type_inst->builtin_primitive;
        return type_inst->type_value;
    }

    if (type_inst->type_dereference_expression) {
        if (type_inst->type_dereference_expression->type == AST_TYPE_INSTANTIATION) {
            return resolve_type_inst(type_inst);
        } else {
            typecheck_expression(type_inst->type_dereference_expression);
        }

        if (compiler->errors_reported) return nullptr;

        auto expr = type_inst->type_dereference_expression;
        while (expr->substitution) expr = expr->substitution;

        auto decl = expr;
        if (expr->type == AST_IDENTIFIER) {
            decl = static_cast<Ast_Identifier *>(expr)->resolved_declaration;
        }

        if (!is_a_type_declaration(decl)) {
            // @TODO it would be nice if this could print out a dereference chain:
            // struct Test { func T (){ ...} ... }
            // Typename expression 'Test.T' doesn't name a type.
            compiler->report_error(expr, "Typename expression doesn't name a type.\n");
            return nullptr;
        }

        if (decl->type == AST_TYPE_ALIAS) {
            auto alias = static_cast<Ast_Type_Alias *>(decl);
            typecheck_expression(alias);
            if (compiler->errors_reported) return nullptr;

            if (!alias->type_value) {
                // We get here when:
                /*
                    struct Reference<T> {}
                    typealias Ref = Reference;
                */
                auto inst = alias->internal_type_inst;

                if (inst->type_dereference_expression) {
                    auto expr = inst->type_dereference_expression;
                    while (expr->substitution) expr = expr->substitution;

                    while (expr->type == AST_IDENTIFIER) {
                        expr = static_cast<Ast_Identifier *>(expr)->resolved_declaration;

                        if (expr->type == AST_TYPE_ALIAS) {
                            auto alias = static_cast<Ast_Type_Alias *>(expr);

                            if (alias->internal_type_inst && alias->internal_type_inst->type_dereference_expression) {
                                expr = alias->internal_type_inst->type_dereference_expression;
                            }
                        }
                    }

                    if (expr->type == AST_STRUCT) {
                        auto _struct = static_cast<Ast_Struct *>(expr);

                        // @Hack so that a template instantiation can get this struct
                        if (_struct->is_template_struct) {
                            return make_struct_type(compiler, _struct);
                        }
                    }
                }
            }

            assert(alias->type_value);
            type_inst->type_value = alias->type_value;
            return type_inst->type_value;
        } else if (decl->type == AST_STRUCT) {
            auto _struct = static_cast<Ast_Struct *>(decl);
            typecheck_expression(_struct);

            // @Hack so that a template instantiation can get this struct
            if (_struct->is_template_struct) {
                return make_struct_type(compiler, _struct);
            }

            type_inst->type_value = _struct->type_value;
            return type_inst->type_value;
        } else if (decl->type == AST_ENUM) {
            auto _enum = static_cast<Ast_Enum *>(decl);
            typecheck_expression(_enum);

            type_inst->type_value = _enum->type_value;
            return type_inst->type_value;
        } else {
            assert(false);
        }
    }

    if (type_inst->template_type_inst_of) {
        auto resolved = resolve_type_inst(type_inst->template_type_inst_of);
        if (compiler->errors_reported) return nullptr;

        if (!resolved || (resolved && !resolved->struct_decl)) {
            compiler->report_error(type_inst, "Attempt to instantiate template type of an expression that does not refer to a struct.\n");
            return nullptr;
        }

        auto _struct = resolved->struct_decl;
        if (!_struct->is_template_struct) {
            compiler->report_error(type_inst, "Struct referred to by template type instantiation is not a template.\n");
            return nullptr;
        }

        auto result = get_polymorph_for_struct(_struct, type_inst->template_type_arguments, type_inst);
        if (!result) {
            compiler->report_error(type_inst, "Could not instantiate template type.\n");
            return nullptr;
        }

        type_inst->type_value = result->type_value;
        return type_inst->type_value;
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

    if (function->return_type) typecheck_expression(function->return_type);

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

    if (!is_for_type_instantiation) {
        for (array_count_type i = 0, j = 1; i < (function->arguments.count - 1) && j < function->arguments.count; ++i, ++j) {
            auto prev = function->arguments[i];
            auto next = function->arguments[j];

            if (prev->initializer_expression && !next->initializer_expression) {
                compiler->report_error(next, "Default argument missing for parameter %d of function.\n", j+1);
                return;
            }
        }

        for (auto arg: function->arguments) {
            if (arg->initializer_expression) {
                if (!folds_to_literal(arg->initializer_expression)) {
                    compiler->report_error(arg->initializer_expression, "Default value for parameter must resolve to a literal value.\n");
                    return;
                }
            }
        }
    }

    function->type_info = compiler->make_function_type(function);
    assert(function->type_info->type == Ast_Type_Info::FUNCTION);

    if (!is_for_type_instantiation && function->linkage_name == to_string("")) {
        if (function->is_c_function) {
            function->linkage_name = function->identifier->name->name;
        } else {
            if (!function->scope && !function->is_intrinsic) {
                compiler->report_error(function, "Function header found without a body. Did you mean to mark this @c_function?\n");
                return;
            }
            function->linkage_name = get_mangled_name(compiler, function);
            // String name = function->linkage_name;
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
            assert(!function->is_intrinsic);
            typecheck_scope(function->scope);
        }

        compiler->function_emission_queue.add(function);
    }
}
