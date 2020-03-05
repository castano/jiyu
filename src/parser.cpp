
#include "parser.h"
#include "compiler.h"
#include <new> // for placement new

#define PARSER_NEW(type) (type *)ast_init(this, new (compiler->get_memory(sizeof(type))) type() );

static
void set_location_info_from_token(Ast *ast, Token *token) {
    ast->text_span = token->text_span;
    ast->filename  = token->filename;
}

static
Ast *ast_init(Parser *parser, Ast *ast) {
    Token *token = parser->peek_token();
    set_location_info_from_token(ast, token);
    return ast;
}

Token *Parser::next_token() {
    return &lexer->tokens[current_token++];
}

Token *Parser::peek_token() {
    return &lexer->tokens[current_token];
}

Ast_Scope *Parser::get_current_scope() {
    return scope_stack[scope_stack.count-1];
}

Ast_Scope *Parser::get_current_canonical_scope() {
    return canonical_scope_stack[canonical_scope_stack.count-1];
}

void Parser::push_scopes(Ast_Scope *scope) {
    scope_stack.add(scope);
    canonical_scope_stack.add(scope);
}

void Parser::pop_scopes() {
    scope_stack.pop();
    canonical_scope_stack.pop();
}

String token_type_to_string(Token::Type type) {
    if (type < Token::END) {
        return mprintf("%c", type);
    }

    switch(type) {
        // These are all copies due to the single-character tokens needing the mprintf above...
        case Token::END:        return copy_string(to_string("eof"));
        case Token::INTEGER:    return copy_string(to_string("integer-literal"));
        case Token::FLOAT:      return copy_string(to_string("float-literal"));
        case Token::IDENTIFIER: return copy_string(to_string("identifier"));
        case Token::STRING:     return copy_string(to_string("string-literal"));

        case Token::KEYWORD_FUNC:      return copy_string(to_string("func"));
        case Token::KEYWORD_VAR:       return copy_string(to_string("var"));
        case Token::KEYWORD_LET:       return copy_string(to_string("let"));
        case Token::KEYWORD_TYPEALIAS: return copy_string(to_string("typealias"));
        case Token::KEYWORD_STRUCT:    return copy_string(to_string("struct"));
        case Token::KEYWORD_UNION:     return copy_string(to_string("union"));
        case Token::KEYWORD_ENUM:      return copy_string(to_string("enum"));
        case Token::KEYWORD_LIBRARY:   return copy_string(to_string("library"));
        case Token::KEYWORD_FRAMEWORK: return copy_string(to_string("framework"));
        case Token::KEYWORD_OPERATOR : return copy_string(to_string("operator"));

        case Token::KEYWORD_IF:     return copy_string(to_string("if"));
        case Token::KEYWORD_ELSE:   return copy_string(to_string("else"));

        case Token::KEYWORD_WHILE:    return copy_string(to_string("while"));
        case Token::KEYWORD_BREAK:    return copy_string(to_string("break"));
        case Token::KEYWORD_CONTINUE: return copy_string(to_string("continue"));
        case Token::KEYWORD_FOR:      return copy_string(to_string("for"));

        case Token::KEYWORD_RETURN: return copy_string(to_string("return"));

        case Token::KEYWORD_VOID:   return copy_string(to_string("void"));
        case Token::KEYWORD_STRING: return copy_string(to_string("string"));
        case Token::KEYWORD_INT:    return copy_string(to_string("int"));
        case Token::KEYWORD_UINT:   return copy_string(to_string("uint"));
        case Token::KEYWORD_UINT8:  return copy_string(to_string("uint8"));
        case Token::KEYWORD_UINT16: return copy_string(to_string("uint16"));
        case Token::KEYWORD_UINT32: return copy_string(to_string("uint32"));
        case Token::KEYWORD_UINT64: return copy_string(to_string("uint64"));
        case Token::KEYWORD_INT8:   return copy_string(to_string("int8"));
        case Token::KEYWORD_INT16:  return copy_string(to_string("int16"));
        case Token::KEYWORD_INT32:  return copy_string(to_string("int32"));
        case Token::KEYWORD_INT64:  return copy_string(to_string("int64"));
        case Token::KEYWORD_FLOAT:  return copy_string(to_string("float"));
        case Token::KEYWORD_DOUBLE: return copy_string(to_string("double"));
        case Token::KEYWORD_BOOL:   return copy_string(to_string("bool"));
        case Token::KEYWORD_TRUE:   return copy_string(to_string("true"));
        case Token::KEYWORD_FALSE:  return copy_string(to_string("false"));
        case Token::KEYWORD_NULL:   return copy_string(to_string("null"));

        case Token::KEYWORD_CAST:   return copy_string(to_string("cast"));
        case Token::KEYWORD_SIZEOF: return copy_string(to_string("sizeof"));
        case Token::KEYWORD_TYPEOF: return copy_string(to_string("typeof"));

        case Token::TAG_C_FUNCTION: return copy_string(to_string("@c_function"));
        case Token::TAG_META:       return copy_string(to_string("@metaprogram"));
        case Token::TAG_EXPORT:     return copy_string(to_string("@export"));
        case Token::TAG_FLAGS:      return copy_string(to_string("@flags"));

        case Token::TEMPORARY_KEYWORD_C_VARARGS: return copy_string(to_string("temporary_c_vararg"));

        case Token::GE_OP:                return copy_string(to_string(">="));
        case Token::LE_OP:                return copy_string(to_string("<="));
        case Token::NE_OP:                return copy_string(to_string("!="));
        case Token::EQ_OP:                return copy_string(to_string("=="));
        case Token::AND_OP:               return copy_string(to_string("&&"));
        case Token::XOR_OP:               return copy_string(to_string("^^"));
        case Token::OR_OP:                return copy_string(to_string("||"));
        case Token::ARROW:                return copy_string(to_string("->"));
        case Token::DEREFERENCE_OR_SHIFT: return copy_string(to_string("<<"));
        case Token::RIGHT_SHIFT:          return copy_string(to_string(">>"));

        case Token::PLUS_EQ:              return copy_string(to_string("+="));
        case Token::MINUS_EQ:             return copy_string(to_string("-="));
        case Token::STAR_EQ:              return copy_string(to_string("*="));
        case Token::SLASH_EQ:             return copy_string(to_string("/="));
        case Token::PERCENT_EQ:           return copy_string(to_string("%="));
        case Token::AMPERSAND_EQ:         return copy_string(to_string("&="));
        case Token::VERTICAL_BAR_EQ:      return copy_string(to_string("|="));
        case Token::CARET_EQ:             return copy_string(to_string("^="));

        case Token::DOTDOT:               return copy_string(to_string(".."));
        case Token::DOTDOTLT:             return copy_string(to_string("..<"));

        default: return String(); // error ?
    }
}

bool Parser::expect(Token::Type type) {
    Token *token = peek_token();

    if (token->type != type) {
        String wanted = token_type_to_string(type);
        String got    = token_type_to_string(token->type);
        compiler->report_error(token, "Expected '%.*s' but got '%.*s'.\n", wanted.length, wanted.data, got.length, got.data);
        free(wanted.data);
        free(got.data);
        return false;
    }

    return true;
}

bool Parser::expect_and_eat(Token::Type type) {
    bool result = expect(type);
    next_token();
    return result;
}

Ast_Identifier *Parser::parse_identifier() {
    if (!expect(Token::IDENTIFIER)) return nullptr;
    Ast_Identifier *ident = PARSER_NEW(Ast_Identifier);

    Token *token = next_token();
    String name = token->string;

    Atom *atom = compiler->make_atom(name);
    assert(atom);

    ident->name = atom;
    ident->enclosing_scope = get_current_scope();
    return ident;
}

Ast_Expression *Parser::parse_primary_expression() {
    Token *token = peek_token();

    if (token->type == Token::IDENTIFIER) {
        auto ident = parse_identifier();
        return ident;
    }

    // @@ Do not hardcode type keyword ranges.
    if (token->type >= Token::KEYWORD_VOID && token->type <= Token::KEYWORD_BOOL) {
        auto type_inst = parse_type_inst();
        return type_inst;
    }

    if (token->type == Token::INTEGER) {
        Ast_Literal *lit = PARSER_NEW(Ast_Literal);
        next_token();

        lit->literal_type = Ast_Literal::INTEGER;
        lit->integer_value = token->integer;
        return lit;
    }

    if (token->type == Token::FLOAT) {
        Ast_Literal *lit = PARSER_NEW(Ast_Literal);
        next_token();

        lit->literal_type = Ast_Literal::FLOAT;
        lit->float_value  = token->_float;
        return lit;
    }

    if (token->type == Token::KEYWORD_TRUE || token->type == Token::KEYWORD_FALSE) {
        Ast_Literal *lit = PARSER_NEW(Ast_Literal);
        next_token();

        lit->literal_type = Ast_Literal::BOOL;
        lit->bool_value = (token->type == Token::KEYWORD_TRUE);
        return lit;
    }

    if (token->type == Token::STRING) {
        Ast_Literal *lit = PARSER_NEW(Ast_Literal);
        next_token();

        lit->literal_type = Ast_Literal::STRING;
        lit->string_value = token->string;
        return lit;
    }

    if (token->type == Token::KEYWORD_NULL) {
        Ast_Literal *lit = PARSER_NEW(Ast_Literal);
        next_token();

        lit->literal_type = Ast_Literal::NULLPTR;
        return lit;
    }

    if (token->type == Token::LEFT_PAREN) {
        next_token();

        auto expr = parse_expression();
        Array<Ast_Expression *> tuple_args;
        tuple_args.add(expr);

        while (peek_token()->type == Token::COMMA) {
            next_token();

            auto expr = parse_expression();
            tuple_args.add(expr);
        }

        if (!expect_and_eat(Token::RIGHT_PAREN)) return nullptr;

        if (tuple_args.count == 1) return expr;

        Ast_Tuple_Expression *tuple = PARSER_NEW(Ast_Tuple_Expression);
        for (auto arg: tuple_args) {
            tuple->arguments.add(arg);
        }

        return tuple;
    }

    if (token->type == Token::KEYWORD_SIZEOF || token->type == Token::KEYWORD_STRIDEOF || token->type == Token::KEYWORD_ALIGNOF) {
        Ast_Sizeof *size = PARSER_NEW(Ast_Sizeof);
        size->operator_type = token->type;
        next_token();

        if (!expect_and_eat(Token::LEFT_PAREN)) return size;

        size->target_type_inst = parse_type_inst();

        if (!expect_and_eat(Token::RIGHT_PAREN)) return size;

        return size;
    }

    if (token->type == Token::KEYWORD_TYPEOF) {
        Ast_Typeof *type_of = PARSER_NEW(Ast_Typeof);
        next_token();
        
        if (!expect_and_eat(Token::LEFT_PAREN)) return type_of;
        
        type_of->expression = parse_expression();
        
        if (!expect_and_eat(Token::RIGHT_PAREN)) return type_of;
        
        return type_of;
    }

    return nullptr;
}

Ast_Expression *Parser::parse_postfix_expression() {
    Ast_Expression *sub_expression = parse_primary_expression();
    if (!sub_expression) return nullptr;

    Token *token = peek_token();
    while (token->type != Token::END) {

        if (token->type == Token::LEFT_PAREN) {
            // transform this into a function call
            Ast_Function_Call *call = PARSER_NEW(Ast_Function_Call);
            copy_location_info(call, sub_expression);
            next_token();

            call->function_or_function_ptr = sub_expression;

            token = peek_token();
            while (token->type != Token::END) {

                if (call->argument_list.count > 0 && token->type == Token::COMMA) {
                    next_token();
                } else if (token->type == Token::RIGHT_PAREN) {
                    break;
                } else if (call->argument_list.count > 0 && token->type != Token::COMMA) {
                    compiler->report_error(call->argument_list[call->argument_list.count-1], "Expected ',' while parsing function-call argument list, but got something else.\n");
                    return call;
                }

                auto expr = parse_expression();
                if (!expr) {
                    // @FixME report_error
                    compiler->report_error(call, "Malformed expression found while parsing parameter list.\n");
                    return nullptr;
                }
                call->argument_list.add(expr);

                token = peek_token();
            }

            if (!expect_and_eat(Token::RIGHT_PAREN)) return nullptr;

            sub_expression = call;
        } else if (token->type == Token::DOT) {
            Ast_Dereference *deref = PARSER_NEW(Ast_Dereference);
            next_token();

            // @TODO do other languages let you use anything other than an identifier for a field selection?
            auto right = parse_identifier();
            if (!right) return nullptr;

            deref->left = sub_expression;
            deref->field_selector = right;

            sub_expression = deref;
        } else if (token->type == '[') {
            Ast_Array_Dereference *deref = PARSER_NEW(Ast_Array_Dereference);

            next_token();
            deref->array_or_pointer_expression = sub_expression;
            deref->index_expression = parse_expression();
            deref->enclosing_scope = get_current_scope();

            if (!expect_and_eat((Token::Type) ']')) return nullptr;

            sub_expression = deref;
        } else {
            break;
        }


        token = peek_token();
    }

    return sub_expression;
}

Ast_Expression *Parser::parse_unary_expression() {
    Token *token = peek_token();

    if (token->type == Token::STAR  ||
        token->type == Token::DEREFERENCE_OR_SHIFT ||
        token->type == Token::MINUS ||
        token->type == Token::EXCLAMATION ||
        token->type == Token::TILDE) {
        Ast_Unary_Expression *ref = PARSER_NEW(Ast_Unary_Expression);
        ref->operator_type = token->type;
        ref->enclosing_scope = get_current_scope();

        next_token();

        // we recurse through parse_unary_expression here, but we may be better off using a loop
        auto expression = parse_unary_expression();
        if (!expression) {
            compiler->report_error(token, "Malformed expression following unary operator '%d'.\n", token->type);
            return nullptr;
        }

        ref->expression = expression;
        return ref;
    } else if (token->type == Token::KEYWORD_CAST) {
        Ast_Cast *cast = PARSER_NEW(Ast_Cast);

        next_token();

        if (!expect_and_eat(Token::LEFT_PAREN)) return nullptr;

        cast->target_type_inst = parse_type_inst();

        if (!expect_and_eat(Token::RIGHT_PAREN)) return nullptr;

        cast->expression = parse_unary_expression();
        if (!cast->expression) {
            compiler->report_error(cast, "Malformed expression following cast.\n", token->type);
            return nullptr;
        }

        return cast;
    } else if (token->type == Token::DOT) {
        Ast_Dereference * deref = PARSER_NEW(Ast_Dereference);
        next_token();

        // @TODO do other languages let you use anything other than an identifier for a field selection?
        auto right = parse_identifier();
        if (!right) return nullptr;

        deref->left = nullptr;          // When this is null, we try to infer it in semantic analysis.
        deref->field_selector = right;

        return deref;
    }

    return parse_postfix_expression();
}

Ast_Expression *Parser::parse_multiplicative_expression() {
    auto sub_expression = parse_unary_expression();
    if (!sub_expression) return nullptr;

    Token *token = peek_token();
    while (token->type != Token::END) {

        if (token->type == Token::STAR
            || token->type == Token::SLASH
            || token->type == Token::PERCENT) {
            Ast_Binary_Expression *bin = PARSER_NEW(Ast_Binary_Expression);
            next_token();

            bin->operator_type = token->type;
            bin->left = sub_expression;
            bin->enclosing_scope = get_current_scope();

            auto right = parse_unary_expression();
            if (!right) {
                compiler->report_error(token, "Malformed expression following '%c' operator.\n", token->type);
                return nullptr;
            }
            bin->right = right;

            sub_expression = bin;
        } else {
            break;
        }

        token = peek_token();
    }

    return sub_expression;
}

Ast_Expression *Parser::parse_additive_expression() {
    auto sub_expression = parse_multiplicative_expression();
    if (!sub_expression) return nullptr;

    Token *token = peek_token();
    while (token->type != Token::END) {

        if (token->type == Token::PLUS
            || token->type == Token::MINUS) {
            Ast_Binary_Expression *bin = PARSER_NEW(Ast_Binary_Expression);
            next_token();

            bin->operator_type = token->type;
            bin->left = sub_expression;
            bin->enclosing_scope = get_current_scope();

            auto right = parse_multiplicative_expression();
            if (!right) {
                compiler->report_error(token, "Malformed expression following '%c' operator.\n", token->type);
                return nullptr;
            }
            bin->right = right;

            sub_expression = bin;
        } else {
            break;
        }

        token = peek_token();
    }

    return sub_expression;
}

Ast_Expression *Parser::parse_shift_expression() {
    auto sub_expression = parse_additive_expression();
    if (!sub_expression) return nullptr;

    Token *token = peek_token();
    while (token->type != Token::END) {

        if (token->type == Token::DEREFERENCE_OR_SHIFT
            || token->type == Token::RIGHT_SHIFT) {
            Ast_Binary_Expression *bin = PARSER_NEW(Ast_Binary_Expression);
            bin->operator_type = token->type;
            bin->left = sub_expression;

            next_token();
            bin->enclosing_scope = get_current_scope();

            auto right = parse_additive_expression();
            if (!right) {
                compiler->report_error(token, "Malformed expression following '%c' operator.\n", token->type);
                return nullptr;
            }
            bin->right = right;

            sub_expression = bin;
        } else {
            break;
        }

        token = peek_token();
    }

    return sub_expression;
}

Ast_Expression *Parser::parse_relational_expression() {
    auto sub_expression = parse_shift_expression();
    if (!sub_expression) return nullptr;

    Token *token = peek_token();
    while (token->type != Token::END) {

        if (token->type == Token::LEFT_ANGLE
            || token->type == Token::RIGHT_ANGLE
            || token->type == Token::LE_OP
            || token->type == Token::GE_OP) {
            Ast_Binary_Expression *bin = PARSER_NEW(Ast_Binary_Expression);
            bin->operator_type = token->type;
            bin->left = sub_expression;

            next_token();

            bin->enclosing_scope = get_current_scope();

            auto right = parse_shift_expression();
            if (!right) {
                compiler->report_error(token, "Malformed expression following '%c' operator.\n", token->type);
                return nullptr;
            }
            bin->right = right;

            sub_expression = bin;
        } else {
            break;
        }

        token = peek_token();
    }

    return sub_expression;
}

Ast_Expression *Parser::parse_equality_expression() {
    auto sub_expression = parse_relational_expression();
    if (!sub_expression) return nullptr;

    Token *token = peek_token();
    while (token->type != Token::END) {

        if (token->type == Token::EQ_OP
            || token->type == Token::NE_OP) {
            next_token();

            Ast_Binary_Expression *bin = PARSER_NEW(Ast_Binary_Expression);
            bin->operator_type = token->type;
            bin->left = sub_expression;
            bin->enclosing_scope = get_current_scope();

            auto right = parse_relational_expression();
            if (!right) {
                auto token_string = token_type_to_string(token->type);
                defer { free(token_string.data); };
                compiler->report_error(token, "Malformed expression following '%.*s' operator.\n", PRINT_ARG(token_string));
                return nullptr;
            }
            bin->right = right;

            sub_expression = bin;
        } else {
            break;
        }

        token = peek_token();
    }

    return sub_expression;
}

Ast_Expression *Parser::parse_and_expression() {
    auto sub_expression = parse_equality_expression();
    if (!sub_expression) return nullptr;

    Token *token = peek_token();
    while (token->type != Token::END) {

        if (token->type == Token::AMPERSAND) {
            Ast_Binary_Expression *bin = PARSER_NEW(Ast_Binary_Expression);
            next_token();

            bin->operator_type = token->type;
            bin->left = sub_expression;
            bin->enclosing_scope = get_current_scope();

            auto right = parse_equality_expression();
            if (!right) {
                auto token_string = token_type_to_string(token->type);
                defer { free(token_string.data); };
                compiler->report_error(token, "Malformed expression following '%.*s' operator.\n", PRINT_ARG(token_string));
                return nullptr;
            }
            bin->right = right;

            sub_expression = bin;
        } else {
            break;
        }

        token = peek_token();
    }

    return sub_expression;
}

Ast_Expression *Parser::parse_exclusive_or_expression() {
    auto sub_expression = parse_and_expression();
    if (!sub_expression) return nullptr;

    Token *token = peek_token();
    while (token->type != Token::END) {

        if (token->type == Token::CARET) {
            Ast_Binary_Expression *bin = PARSER_NEW(Ast_Binary_Expression);
            next_token();

            bin->operator_type = token->type;
            bin->left = sub_expression;
            bin->enclosing_scope = get_current_scope();

            auto right = parse_and_expression();
            if (!right) {
                auto token_string = token_type_to_string(token->type);
                defer { free(token_string.data); };
                compiler->report_error(token, "Malformed expression following '%.*s' operator.\n", PRINT_ARG(token_string));
                return nullptr;
            }
            bin->right = right;

            sub_expression = bin;
        } else {
            break;
        }

        token = peek_token();
    }

    return sub_expression;
}

Ast_Expression *Parser::parse_inclusive_or_expression() {
    auto sub_expression = parse_exclusive_or_expression();
    if (!sub_expression) return nullptr;

    Token *token = peek_token();
    while (token->type != Token::END) {

        if (token->type == Token::VERTICAL_BAR) {
            Ast_Binary_Expression *bin = PARSER_NEW(Ast_Binary_Expression);
            next_token();

            bin->operator_type = token->type;
            bin->left = sub_expression;
            bin->enclosing_scope = get_current_scope();

            auto right = parse_exclusive_or_expression();
            if (!right) {
                auto token_string = token_type_to_string(token->type);
                defer { free(token_string.data); };
                compiler->report_error(token, "Malformed expression following '%.*s' operator.\n", PRINT_ARG(token_string));
                return nullptr;
            }
            bin->right = right;

            sub_expression = bin;
        } else {
            break;
        }

        token = peek_token();
    }

    return sub_expression;
}

Ast_Expression *Parser::parse_logical_and_expression() {
    auto sub_expression = parse_inclusive_or_expression();
    if (!sub_expression) return nullptr;

    Token *token = peek_token();
    while (token->type != Token::END) {

        if (token->type == Token::AND_OP) {
            Ast_Binary_Expression *bin = PARSER_NEW(Ast_Binary_Expression);
            bin->operator_type = token->type;
            bin->left = sub_expression;

            next_token();
            bin->enclosing_scope = get_current_scope();

            auto right = parse_inclusive_or_expression();
            if (!right) {
                auto token_string = token_type_to_string(token->type);
                defer { free(token_string.data); };
                compiler->report_error(token, "Malformed expression following '%.*s' operator.\n", PRINT_ARG(token_string));
                return nullptr;
            }
            bin->right = right;

            sub_expression = bin;
        } else {
            break;
        }

        token = peek_token();
    }

    return sub_expression;
}

Ast_Expression *Parser::parse_logical_xor_expression() {
    auto sub_expression = parse_logical_and_expression();
    if (!sub_expression) return nullptr;

    Token *token = peek_token();
    while (token->type != Token::END) {

        if (token->type == Token::XOR_OP) {
            Ast_Binary_Expression *bin = PARSER_NEW(Ast_Binary_Expression);
            bin->operator_type = token->type;
            bin->left = sub_expression;

            next_token();
            bin->enclosing_scope = get_current_scope();

            auto right = parse_logical_and_expression();
            if (!right) {
                auto token_string = token_type_to_string(token->type);
                defer { free(token_string.data); };
                compiler->report_error(token, "Malformed expression following '%.*s' operator.\n", PRINT_ARG(token_string));
                return nullptr;
            }
            bin->right = right;

            sub_expression = bin;
        } else {
            break;
        }

        token = peek_token();
    }

    return sub_expression;
}

Ast_Expression *Parser::parse_logical_or_expression() {
    auto sub_expression = parse_logical_xor_expression();
    if (!sub_expression) return nullptr;

    Token *token = peek_token();
    while (token->type != Token::END) {

        if (token->type == Token::OR_OP) {
            Ast_Binary_Expression *bin = PARSER_NEW(Ast_Binary_Expression);
            bin->operator_type = token->type;
            bin->left = sub_expression;

            next_token();
            bin->enclosing_scope = get_current_scope();

            auto right = parse_logical_xor_expression();
            if (!right) {
                auto token_string = token_type_to_string(token->type);
                defer { free(token_string.data); };
                compiler->report_error(token, "Malformed expression following '%.*s' operator.\n", PRINT_ARG(token_string));
                return nullptr;
            }
            bin->right = right;

            sub_expression = bin;
        } else {
            break;
        }

        token = peek_token();
    }

    return sub_expression;
}

Ast_Expression *Parser::parse_expression() {
    return parse_logical_or_expression();
}

Ast_Expression *Parser::parse_statement() {
    Token *token = peek_token();

    if (token->type == Token::KEYWORD_FUNC || token->type == Token::KEYWORD_OPERATOR) {
        return parse_function();
    }

    if (token->type == Token::KEYWORD_LIBRARY || token->type == Token::KEYWORD_FRAMEWORK) {
        Ast_Library *lib = PARSER_NEW(Ast_Library);
        lib->is_framework = (token->type == Token::KEYWORD_FRAMEWORK);

        next_token();

        token = peek_token();
        if (!expect_and_eat(Token::STRING)) return nullptr;

        lib->libname = token->string;

        if (!expect_and_eat(Token::SEMICOLON)) return nullptr;
        return lib;
    }

    if (token->type == Token::KEYWORD_TYPEALIAS) {
        Ast_Type_Alias *alias = PARSER_NEW(Ast_Type_Alias);
        next_token();

        if (peek_token()->type == Token::TAG_DISTINCT) {
            alias->is_distinct = true;
            next_token();
        }

        alias->identifier = parse_identifier();

        if (!expect_and_eat(Token::EQUALS)) return nullptr;

        alias->internal_type_inst = parse_type_inst();

        if (!alias->internal_type_inst) {
            compiler->report_error(alias, "Could not parse aliasee following typealias.\n");
            return nullptr;
        }

        if (!expect_and_eat(Token::SEMICOLON)) return nullptr;

        return alias;
    }

    if (token->type == Token::KEYWORD_STRUCT || token->type == Token::KEYWORD_UNION) {
        Ast_Struct *_struct = PARSER_NEW(Ast_Struct);
        next_token();

        if (peek_token()->type != Token::IDENTIFIER) {
            _struct->is_anonymous = true;
        } else {
            _struct->identifier = parse_identifier();
        }
        _struct->member_scope.parent = get_current_scope();
        _struct->member_scope.owning_struct = _struct;
        _struct->is_union = (token->type == Token::KEYWORD_UNION);

        if (peek_token()->type == Token::LEFT_ANGLE) {
            Ast_Scope *polymorphic_scope = PARSER_NEW(Ast_Scope);
            polymorphic_scope->is_template_argument_block = true;
            polymorphic_scope->parent = get_current_scope();
            _struct->polymorphic_type_alias_scope = polymorphic_scope;
            next_token();

            push_scopes(polymorphic_scope);

            token = peek_token();
            while (token->type != Token::END) {
                Ast_Type_Alias *alias = PARSER_NEW(Ast_Type_Alias);
                alias->identifier = parse_identifier();

                if (!alias->identifier) {
                    compiler->report_error(alias, "Expected identifier in template argument list but got something else.\n");
                    return nullptr;
                }

                polymorphic_scope->declarations.add(alias);

                token = peek_token();
                if (token->type == Token::COMMA) {
                    next_token();

                    token = peek_token();
                    continue;
                }

                if (!expect_and_eat(Token::RIGHT_ANGLE)) return nullptr;

                break;
            }

            pop_scopes();

            _struct->member_scope.parent = polymorphic_scope;

            _struct->is_template_struct = true;
        }

        if (peek_token()->type == Token::COLON) {
            next_token();

            _struct->parent_struct = parse_type_inst();
        }

        set_location_info_from_token(&_struct->member_scope, peek_token());
        parse_scope(&_struct->member_scope, true);
        return _struct;
    }

    if (token->type == Token::KEYWORD_ENUM) {
        Ast_Enum *_enum = PARSER_NEW(Ast_Enum);

        next_token();
        
        Token * token = peek_token();
        if (token->type == Token::TAG_FLAGS) {
            _enum->is_flags = true;
            next_token();
        }
        _enum->identifier = parse_identifier();

        token = peek_token();
        if (token->type == Token::COLON) {
            next_token();

            Ast_Type_Instantiation *type_inst = parse_type_inst();
            if (!type_inst) return nullptr;

            // Make sure it's an integer type.
            if (!type_inst->builtin_primitive || type_inst->builtin_primitive->type != Ast_Type_Info::INTEGER) {
                compiler->report_error(type_inst, "Expected integer type.\n");
            }

            _enum->base_type = type_inst;
        }
        else {
            // If not type provided, assume uint32.
            _enum->base_type = wrap_primitive_type(compiler->type_uint32);
        }

        _enum->enum_type_inst = PARSER_NEW(Ast_Type_Instantiation);
        _enum->enum_type_inst->type_dereference_expression = _enum->identifier;

        _enum->member_scope.parent = get_current_scope();
        _enum->member_scope.owning_enum = _enum;
        parse_enum_scope(&_enum->member_scope);

        return _enum;
    }

    if (token->type == Token::KEYWORD_VAR) {
        auto var = parse_variable_declaration(true);
        if (!expect_and_eat(Token::SEMICOLON)) return nullptr;
        return var;
    }

    if (token->type == Token::KEYWORD_LET) {
        next_token();

        auto let = parse_variable_declaration(false);
        if (!expect_and_eat(Token::SEMICOLON)) return nullptr;

        let->is_let = true;
        return let;
    }

    if (token->type == Token::KEYWORD_IF) {
        Ast_If *_if = PARSER_NEW(Ast_If);
        next_token();

        _if->condition = parse_expression();
        if (compiler->errors_reported) return _if;

        if (!_if->condition) {
            // @Cleanup move to sema?
            compiler->report_error(_if, "'if' must be followed by an expression.\n");
            return _if;
        }

        set_location_info_from_token(&_if->then_scope, peek_token());
        _if->then_scope.parent = get_current_scope();
        parse_scope(&_if->then_scope, false, true);

        token = peek_token();
        if (token->type == Token::KEYWORD_ELSE) {
            next_token();

            _if->else_scope = PARSER_NEW(Ast_Scope);
            _if->else_scope->parent = get_current_scope();
            parse_scope(_if->else_scope, false, true);
        }

        return _if;
    }

    if (token->type == Token::KEYWORD_SWITCH) {
        Ast_Switch *_switch = PARSER_NEW(Ast_Switch);
        next_token();

        _switch->condition = parse_expression();
        if (compiler->errors_reported) return _switch;

        if (!_switch->condition) {
            // @Cleanup move to sema?
            compiler->report_error(_switch, "'switch' must be followed by an expression.\n");
            return _switch;
        }

        set_location_info_from_token(&_switch->scope, peek_token());
        _switch->scope.parent = get_current_scope();
        _switch->scope.owning_statement = _switch;
        parse_scope(&_switch->scope, true);
        return _switch;
    }

    if (token->type == Token::KEYWORD_CASE) {
        Ast_Case *_case = PARSER_NEW(Ast_Case);
        next_token();

        while (true) {
            Ast_Expression *expr = parse_expression();
            if (compiler->errors_reported) return nullptr;

            if (!expr) {
                compiler->report_error(_case, "Empty expression following 'case' statement");
                return nullptr;
            }

            _case->conditions.add(expr);

            if (peek_token()->type == Token::COMMA) {
                next_token();
                continue;
            }

            break;
        }

        if (!expect_and_eat(Token::COLON)) return nullptr;

        set_location_info_from_token(&_case->scope, peek_token());
        _case->scope.parent = get_current_scope();
        parse_scope(&_case->scope, /*requires_braces*/false, /*only_one_statement*/false, /*push_scope*/true, /*is_for_case*/true);

        return _case;
    }

    if (token->type == Token::KEYWORD_FOR) {
        Ast_For *_for = PARSER_NEW(Ast_For);
        next_token();

        token = peek_token();
        if (token->type == Token::STAR) {
            next_token();

            _for->is_element_pointer_iteration = true;
        }

        // We currently support for loops with these formats:

        // for a..b {}
        // for array {}
        // for it in a..b {}
        // for it in array {}

        // In the future we may also add:
        // for it_index,it in array {}

        // This could be an array, the first part of a range, or an iterator identifier.
        auto first_expression = parse_expression();

        // If we encounter "in", then the firt expression was an iterator identifier.
        // We treat "in" as a contextual keyword, instead of a reserved token, that way we can use the "in" identifier in other contexts.
        
        Ast_Identifier * ident = nullptr;
        if (first_expression->type == Ast_Type::AST_IDENTIFIER) {
            ident = static_cast<Ast_Identifier *>(first_expression);
        }


        token = peek_token();
        if (token->type == Token::IDENTIFIER && token->string == to_string("in")) {
            if (ident == nullptr) {
                compiler->report_error(first_expression, "Invalid iterator declaration.\n");
                return _for;
            }
            // For clarity, it's ilegal to name your iterator 'in'.
            if (ident->name->name == to_string("in")) {
                compiler->report_error(ident, "Invalid iterator declaration.\n");
                return _for;
            }

            _for->iterator_ident = ident;

            next_token();

            first_expression = parse_expression();
            token = peek_token();
        }
        else {
            if (ident && ident->name->name == to_string("in")) {
                compiler->report_error(ident, "'in' must be preceeded by an iterator declaration.\n");
                return _for;
            }
        }

        _for->initial_iterator_expression = first_expression;
        
        if (token->type == Token::DOTDOT || token->type == Token::DOTDOTLT) {
            if (token->type == Token::DOTDOTLT) _for->is_exclusive_end = true;
            next_token();

            if (!_for->initial_iterator_expression) {
                compiler->report_error(token, ".. operator must be preceeded by an expression.\n");
                return _for;
            }

            _for->upper_range_expression = parse_expression();
        }

        _for->iterator_declaration_scope.parent = get_current_scope();
        _for->body.parent = &_for->iterator_declaration_scope;
        _for->body.owning_statement = _for;
        set_location_info_from_token(&_for->body, peek_token());
        parse_scope(&_for->body, false, true);
        return _for;
    }

    if (token->type == Token::KEYWORD_BREAK || token->type == Token::KEYWORD_CONTINUE) {
        Ast_Control_Flow *flow = PARSER_NEW(Ast_Control_Flow);
        flow->control_type  = token->type;
        flow->current_scope = get_current_scope();

        // @Incomplete parse possible identifier for break-ing to some scope outside the most immediate loop.

        next_token();
        if (!expect_and_eat(Token::SEMICOLON)) return nullptr;
        return flow;
    }

    if (token->type == Token::KEYWORD_WHILE) {
        Ast_While *loop = PARSER_NEW(Ast_While);
        next_token();

        loop->condition = parse_expression();

        if (!loop->condition) {
            compiler->report_error(loop, "'while' must be followed by an expression.\n");
            return loop;
        }

        loop->body.parent = get_current_scope();
        loop->body.owning_statement = loop;
        set_location_info_from_token(&loop->body, peek_token());
        parse_scope(&loop->body, false, true);
        return loop;
    }

    if (token->type == Token::KEYWORD_RETURN) {
        Ast_Return *ret = PARSER_NEW(Ast_Return);
        next_token();

        ret->owning_function = currently_parsing_function;
        ret->expression = parse_expression();

        if (!expect_and_eat(Token::SEMICOLON)) return nullptr;
        return ret;
    }

    if (token->type == '#') {
        next_token();

        token = peek_token();

        if (token->type == Token::IDENTIFIER && token->string == to_string("load")) {
            if (!expect_and_eat(Token::IDENTIFIER)) return nullptr;

            Ast_Directive_Load *load = PARSER_NEW(Ast_Directive_Load);
            load->scope_i_belong_to = get_current_canonical_scope();
            compiler->queue_directive(load);

            token = peek_token();
            String name = token->string;
            String base_path = basepath(lexer->filename);

            if (!expect_and_eat(Token::STRING)) return nullptr;
            if (!expect_and_eat(Token::SEMICOLON)) return nullptr;

            const int MAX_PATH = 512;
            char fullname[MAX_PATH];
            snprintf(fullname, MAX_PATH, "%.*s%.*s", PRINT_ARG(base_path), PRINT_ARG(name));

            load->target_filename = copy_string(to_string(fullname));
            load->target_scope    = get_current_scope();
            return load;
        } else if (token->type == Token::IDENTIFIER && token->string == to_string("import")) {
            if (!expect_and_eat(Token::IDENTIFIER)) return nullptr;

            Ast_Directive_Import *import = PARSER_NEW(Ast_Directive_Import);
            import->scope_i_belong_to = get_current_canonical_scope();
            compiler->queue_directive(import);

            token = peek_token();
            String name = token->string;

            if (!expect_and_eat(Token::STRING)) return nullptr;
            if (!expect_and_eat(Token::SEMICOLON)) return nullptr;

            import->target_filename = copy_string(name); // fullname will be resolved when the directive is resolved.
            import->target_scope    = get_current_scope();
            return import;
        } else if (token->type == Token::KEYWORD_IF) {
            Ast_Directive_Static_If *_if = PARSER_NEW(Ast_Directive_Static_If);
            if (!expect_and_eat(Token::KEYWORD_IF)) return nullptr;

            _if->scope_i_belong_to = get_current_canonical_scope();
            compiler->queue_directive(_if); // queue the directive early so that further directives that depend on this arent queued first.

            token = peek_token();

            // Prevent identifier lookups outside preload/builtin scope.
            // This is to prevent allowing order-dependent directive resolutions
            // that depend on each other being resolved in specific orders to work.
            // -josh 4 January 2020
            push_scopes(compiler->preload_scope);
            _if->condition = parse_expression();
            pop_scopes();

            _if->then_scope = PARSER_NEW(Ast_Scope);
            _if->then_scope->parent = get_current_canonical_scope();

            canonical_scope_stack.add(_if->then_scope);
            parse_scope(_if->then_scope, true, false, false);
            canonical_scope_stack.pop();

            token = peek_token();
            if (token->type == Token::KEYWORD_ELSE) {
                next_token();

                _if->else_scope = PARSER_NEW(Ast_Scope);
                _if->else_scope->parent = get_current_canonical_scope();

                canonical_scope_stack.add(_if->else_scope);
                parse_scope(_if->else_scope, true, false, false);
                canonical_scope_stack.pop();
            }

            return _if;
        } else if (token->type == Token::IDENTIFIER && token->string == to_string("clang_import")) {
            if (!expect_and_eat(Token::IDENTIFIER)) return nullptr;

            Ast_Directive_Clang_Import *import = PARSER_NEW(Ast_Directive_Clang_Import);
            import->scope_i_belong_to = get_current_canonical_scope();
            compiler->queue_directive(import);

            token = peek_token();
            import->string_to_compile = token->string;

            if (!expect_and_eat(Token::STRING)) return nullptr;
            if (!expect_and_eat(Token::SEMICOLON)) return nullptr;

            import->target_scope    = get_current_scope();
            return import;
        } else {
            String s  = token->string;
            compiler->report_error(token, "Unknown compiler directive '%.*s'.\n", s.length, s.data);
            return nullptr;
        }
    }

    if (token->type == '{') {
        auto parent = get_current_scope();
        Ast_Scope *scope = PARSER_NEW(Ast_Scope);
        scope->parent = parent;
        parse_scope(scope, true);
        return scope;
    }

    if (token->type == '}') {
        return nullptr;
    }


    Ast_Expression *left = parse_expression();

    if (left) {
        token = peek_token();
        if (token->type == Token::EQUALS          ||     // =
            token->type == Token::PLUS_EQ         ||     // +=
            token->type == Token::MINUS_EQ        ||     // -=
            token->type == Token::STAR_EQ         ||     // *=
            token->type == Token::SLASH_EQ        ||     // /=
            token->type == Token::PERCENT_EQ      ||     // %=
            token->type == Token::AMPERSAND_EQ    ||     // &=
            token->type == Token::VERTICAL_BAR_EQ ||     // |=
            token->type == Token::CARET_EQ               // ^=
            ) {
            Ast_Binary_Expression *bin = PARSER_NEW(Ast_Binary_Expression);
            bin->operator_type = token->type;

            next_token();
            bin->enclosing_scope = get_current_scope();

            Ast_Expression *right = parse_expression();
            if (!right) {
                if (!compiler->errors_reported) {
                    compiler->report_error(token, "Right-hand-side of assignment-statement must contain an expression.\n");
                    return nullptr;
                }
            }

            bin->left = left;
            bin->right = right;

            left = bin;
        }

        if (!expect_and_eat(Token::SEMICOLON)) return nullptr;

        return left;
    }
    else {
        String token_name = token_type_to_string(token->type);
        defer { free(token_name.data); };
        compiler->report_error(token, "Unexpected token '%.*s'.\n", PRINT_ARG(token_name));
        return nullptr;
    }
}

static Ast_Expression * find_declaration(Array<Ast_Scope_Entry *> * array, Atom * name) {
    for (auto it: *array) {
        auto id = declaration_identifier(it);
        if (id && id->name == name) {
            return it;
        }
    }
    return nullptr;
}

// @FixMe this should not be here, we do not know if something is actually redefined until after
// directives are resolved. This should be checked in sema. -josh 29 December 2019
bool Parser::add_declaration(Array<Ast_Scope_Entry *> *declarations, Ast_Scope_Entry *decl) {
    auto id = declaration_identifier(decl);

    // Skip anonymous declarations, in case we have them.
    if (id) {
        // Check duplicate declarations.
        auto prev_decl = find_declaration(declarations, id->name);

        if (prev_decl) {
            if (decl->type != AST_FUNCTION || prev_decl->type != AST_FUNCTION) {
                compiler->report_error(id, "Redefinition of '%.*s'.\n", PRINT_ARG(id->name->name));
                compiler->report_error(prev_decl, "previous definition is here:\n");
                return false;
            }
        }
    }

    declarations->add(decl);
    return true;
}

void Parser::parse_scope(Ast_Scope *scope, bool requires_braces, bool only_one_statement, bool push_scope, bool is_for_case) {
    MICROPROFILE_SCOPEI("parser", "parse_scope", -1);

    if (push_scope) push_scopes(scope);

    if (!requires_braces && only_one_statement == true && peek_token()->type == '{') {
        requires_braces    = true;
        only_one_statement = false;
    }

    if (requires_braces && !expect_and_eat((Token::Type) '{')) return;

    Token *token = peek_token();
    while (token->type != Token::END) {

        if (requires_braces && token->type == '}') break;

        // We get here if we are in the process of parsing a scope for a case statement,
        // so if we hit another case token, stop.
        if (is_for_case && peek_token()->type == Token::KEYWORD_CASE) break;

        Ast_Expression *stmt = parse_statement();
        if (stmt) {
            scope->statements.add(stmt);

            if (is_declaration(stmt->type)) {
                if (!add_declaration(&scope->declarations, static_cast<Ast_Scope_Entry *>(stmt))) {
                    return;
                }
            }
        } else {
            // If there are no more statements we can parse (because we have hit the closing brace of the switch),
            // then break.
            if (is_for_case) break;
        }

        if (compiler->errors_reported) return;

        if (only_one_statement) break;

        token = peek_token();
    }

    if (requires_braces && !expect_and_eat((Token::Type) '}')) return;

    if (push_scope) pop_scopes();
}

void Parser::parse_enum_scope(Ast_Scope *scope) {
    assert(scope->owning_enum != nullptr);

    push_scopes(scope);

    if (!expect_and_eat((Token::Type) '{')) return;
    
    Token *token = peek_token();
    while (token->type != Token::END) {
        
        if (token->type == '}') break;
        
        auto decl = parse_variable_declaration(/*expect_var_keyword=*/false, /*enum_value_declaration=*/true);
        if (!expect_and_eat(Token::SEMICOLON)) return;
        
        decl->is_let = true;
        decl->type_inst = scope->owning_enum->enum_type_inst;

        scope->statements.add(decl);
        scope->declarations.add(decl);
        
        if (compiler->errors_reported) return;
                
        token = peek_token();
    }
    
    if (!expect_and_eat((Token::Type) '}')) return;

    pop_scopes();
}


Ast_Declaration *Parser::parse_variable_declaration(bool expect_var_keyword, bool enum_value_declaration) {
    if (expect_var_keyword && !expect_and_eat(Token::KEYWORD_VAR)) return nullptr;

    Token *ident_token = peek_token(); // used for error below, @Cleanup we want to be able to report errors using an Ast
    Ast_Identifier *ident = parse_identifier();
    if (!ident) {
        compiler->report_error(ident_token, "Expected identifier for variable declaration.\n");
        return nullptr;
    }

    Ast_Declaration *decl = PARSER_NEW(Ast_Declaration);
    decl->identifier = ident;
    copy_location_info(decl, ident);

    Token *token = peek_token();
    if (token->type == Token::COLON) {
        next_token();

        Ast_Type_Instantiation *type_inst = parse_type_inst();
        if (!type_inst) return nullptr;


        decl->type_inst = type_inst;
    }

    token = peek_token();
    if (token->type == Token::EQUALS) {
        next_token();

        Ast_Expression *expression = parse_expression();
        if (!expression) {
            if (!compiler->errors_reported) {
                compiler->report_error(peek_token(), "Right-hand-side intialization of declaration must contain an expression.\n");
            }
            return nullptr;
        }

        decl->initializer_expression = expression;
    }

    if (!decl->initializer_expression && !decl->type_inst && !enum_value_declaration) {
        // @TODO maybe this should be moved to semantic analysis
        compiler->report_error(ident_token, "Declared variable must be declared with a type or be initialized.\n");
        return nullptr;
    }


    return decl;
}


Ast_Type_Instantiation *Parser::wrap_primitive_type(Ast_Type_Info *info) {
    assert(info->type == Ast_Type_Info::VOID    ||
           info->type == Ast_Type_Info::BOOL    ||
           info->type == Ast_Type_Info::INTEGER ||
           info->type == Ast_Type_Info::FLOAT   ||
           info->type == Ast_Type_Info::STRING);

    auto type_inst = PARSER_NEW(Ast_Type_Instantiation);
    type_inst->builtin_primitive = info;
    return type_inst;
}

Ast_Type_Instantiation *Parser::parse_type_inst() {
    auto inst = parse_primary_type_inst();

    while (peek_token()->type == Token::LEFT_ANGLE) {
        Ast_Type_Instantiation *_template = PARSER_NEW(Ast_Type_Instantiation);
        _template->template_type_inst_of = inst;

        next_token();

        while (peek_token()->type != Token::END) {
            auto arg = parse_type_inst();

            _template->template_type_arguments.add(arg);

            if (peek_token()->type == Token::COMMA) {
                next_token();
                continue;
            }

            if (peek_token()->type == Token::RIGHT_ANGLE) break;
        }

        if (!expect_and_eat(Token::RIGHT_ANGLE)) return nullptr;

        inst = _template;
    }

    return inst;
}

Ast_Type_Instantiation *Parser::parse_primary_type_inst() {
    Token *token = peek_token();

    Ast_Type_Info *builtin_primitive = nullptr;
    switch (token->type) {
        case Token::KEYWORD_INT:    builtin_primitive = compiler->type_int32; break;  // @IntegerSize ??
        case Token::KEYWORD_UINT:   builtin_primitive = compiler->type_uint32; break; // @IntegerSize ??

        case Token::KEYWORD_INT8:   builtin_primitive = compiler->type_int8; break;
        case Token::KEYWORD_INT16:  builtin_primitive = compiler->type_int16; break;
        case Token::KEYWORD_INT32:  builtin_primitive = compiler->type_int32; break;
        case Token::KEYWORD_INT64:  builtin_primitive = compiler->type_int64; break;

        case Token::KEYWORD_UINT8:  builtin_primitive = compiler->type_uint8; break;
        case Token::KEYWORD_UINT16: builtin_primitive = compiler->type_uint16; break;
        case Token::KEYWORD_UINT32: builtin_primitive = compiler->type_uint32; break;
        case Token::KEYWORD_UINT64: builtin_primitive = compiler->type_uint64; break;

        case Token::KEYWORD_FLOAT:  builtin_primitive = compiler->type_float32; break;
        case Token::KEYWORD_DOUBLE: builtin_primitive = compiler->type_float64; break;

        case Token::KEYWORD_STRING: builtin_primitive = compiler->type_string; break;

        case Token::KEYWORD_VOID:   builtin_primitive = compiler->type_void; break;

        case Token::KEYWORD_BOOL:   builtin_primitive = compiler->type_bool; break;

        default: break; // JH: silence warnings.
    }

    if (builtin_primitive) {
        next_token();
        return wrap_primitive_type(builtin_primitive);
    }

    if (token->type == Token::STAR) {
        next_token();
        auto pointee = parse_type_inst();
        if (!pointee) {
            compiler->report_error(token, "Couldn't parse pointer element type.\n");
            return nullptr;
        }

        Ast_Type_Instantiation *type_inst = PARSER_NEW(Ast_Type_Instantiation);
        type_inst->pointer_to = pointee;
        return type_inst;
    }

    if (token->type == Token::IDENTIFIER) {
        Ast_Type_Instantiation *type_inst = PARSER_NEW(Ast_Type_Instantiation);

        auto ident = parse_identifier();

        Ast_Expression *sub_expression = ident;
        token = peek_token();
        while (token->type == Token::DOT) {
            Ast_Dereference *deref = PARSER_NEW(Ast_Dereference);
            next_token();

            // @TODO do other languages let you use anything other than an identifier for a field selection?
            auto right = parse_identifier();
            if (!right) return nullptr;

            deref->left = sub_expression;
            deref->field_selector = right;

            sub_expression = deref;

            token = peek_token();
        }

        type_inst->type_dereference_expression = sub_expression;
        return type_inst;
    }

    if (token->type == '[') {
        Ast_Type_Instantiation *type_inst = PARSER_NEW(Ast_Type_Instantiation);
        next_token();

        token = peek_token();
        if (token->type == Token::DOTDOT) {
            type_inst->array_is_dynamic = true;
            next_token();

            if (!expect_and_eat((Token::Type) ']')) return type_inst;
        } else if (token->type == ']') {
            next_token();
        } else {
            type_inst->array_size_expression = parse_expression();

            if (!type_inst->array_size_expression) {
                compiler->report_error(type_inst, "Expected expression or '..' token within array size instantiation.\n");
            }

            if (!expect_and_eat((Token::Type) ']')) return type_inst;
        }

        type_inst->array_element_type = parse_type_inst();
        if (!type_inst->array_element_type) {
            compiler->report_error(type_inst, "Couldn't parse array element type.\n");
        }
        return type_inst;
    }

    if (token->type == Token::TAG_C_FUNCTION) {
        next_token();

        // @TODO do we want to restructure this so that one can mix @c_function with a typealias?
        // Ex:
        // typealias My_Func_Type = () -> void;
        // typealias My_C_Func_Type = @c_function My_Func_Type;

        auto func_type_inst = parse_type_inst();
        if (compiler->errors_reported) return nullptr;

        if (!func_type_inst->function_header) {
            compiler->report_error(token, "Tag @c_function may only preceed a function type.\n");
            return nullptr;
        }

        func_type_inst->function_header->is_c_function = true;
        return func_type_inst;
    }

    if (token->type == Token::TAG_META) {
        compiler->report_error(token, "@metaprogram tag is not valid for function types.");
        return nullptr;
    }

    if (token->type == Token::TAG_EXPORT) {
        compiler->report_error(token, "@export tag is not valid for function types.");
        return nullptr;
    }

    if (token->type == Token::LEFT_PAREN) {
        Ast_Type_Instantiation *final_type_inst = PARSER_NEW(Ast_Type_Instantiation);
        next_token();

        Array<Ast_Declaration *> members;
        bool is_c_varargs = false;

        token = peek_token();
        while (token->type != Token::END) {

            if (members.count > 0 && token->type == Token::COMMA) {
                next_token();
                token = peek_token();
            } else  if (token->type == Token::RIGHT_PAREN) break;

            // @Temporary
            // @Temporary
            // @Temporary
            if (token->type == Token::TEMPORARY_KEYWORD_C_VARARGS) {
                next_token();
                is_c_varargs = true;

                token = peek_token();
                if (token->type != Token::RIGHT_PAREN) {
                    compiler->report_error(token, "Expected ')' following 'temporary_c_vararg' declarator.\n");
                    return nullptr;
                }
                break;
            }

            Ast_Declaration *decl = parse_variable_declaration(false);
            if (decl) {
                decl->is_let = true;
                members.add(decl);
            }

            if (compiler->errors_reported) return nullptr;

            token = peek_token();
        }

        if (!expect_and_eat(Token::RIGHT_PAREN)) return nullptr;

        if (peek_token()->type == Token::ARROW) {
            Ast_Function *function = PARSER_NEW(Ast_Function);
            function->is_c_varargs = is_c_varargs;

            for (auto arg: members) function->arguments.add(arg);

            token = peek_token();
            if (!expect_and_eat(Token::ARROW)) return nullptr;

            Ast_Type_Instantiation *type_inst = parse_type_inst();
            if (!type_inst) {
                compiler->report_error(token, "Could not parse type following '->'.\n");
                return nullptr;
            }

            function->return_type = type_inst;

            token = peek_token();

            final_type_inst->function_header = function;
            return final_type_inst;
        } else {
            Ast_Struct *tuple = PARSER_NEW(Ast_Struct);
            tuple->is_tuple = true;

            for (auto mem: members) {
                mem->is_let = false; // @Hack since this gets set in the loop above
                // @Incomplete right now we require you to name each field because of how we're parsing the declarations
                // we will want a way to detect that a name is given in order to assign a default name
                tuple->member_scope.declarations.add(mem);
            }

            Ast_Type_Instantiation *inst  = PARSER_NEW(Ast_Type_Instantiation);
            inst->type_dereference_expression = tuple; // Tiny @Hack to stuff this tuple struct into Type_Instantiation, we'll want to do something similar for anon structs.
            return inst;
        }

        return nullptr;
    }

    return nullptr;
}

bool is_tag_token(Token *token) {
    auto type = token->type;
    return type == Token::TAG_C_FUNCTION || type == Token::TAG_META
        || type == Token::TAG_EXPORT || type == Token::TAG_FLAGS;
}

Ast_Function *Parser::parse_function() {
    bool is_operator_function = (peek_token()->type == Token::KEYWORD_OPERATOR);

    if (is_operator_function) expect_and_eat(Token::KEYWORD_OPERATOR);
    else expect_and_eat(Token::KEYWORD_FUNC);

    Ast_Function *function = PARSER_NEW(Ast_Function);
    function->is_operator_function = is_operator_function;
    function->arguments_scope.parent = get_current_scope();

    Ast_Function *old_function = currently_parsing_function;

    currently_parsing_function = function;


    Token *token = peek_token();

    // @Incomplete disallow @export when @c_function is used, and vice versa.
    while (is_tag_token(token)) {
        if (token->type == Token::TAG_C_FUNCTION) {
            function->is_c_function = true;
            next_token();

            token = peek_token();
            if (token->type == Token::LEFT_PAREN) {
                next_token();

                // We expect a string here instead of an identifier because the user may need punctuation in the symbol name.
                // func @c_function("linkage_name") my_function();
                if (!expect(Token::STRING)) return nullptr;

                token = peek_token();
                function->linkage_name = token->string;
                next_token();

                if (!expect_and_eat(Token::RIGHT_PAREN)) return nullptr;
            }
        } else if (token->type == Token::TAG_META) {
            function->is_marked_metaprogram = true;
            next_token();
        } else if (token->type == Token::TAG_EXPORT) {
            function->is_exported = true;

            next_token();
            if (!expect_and_eat(Token::LEFT_PAREN)) return nullptr;

            // We expect a string here instead of an identifier because the user may need punctuation in the symbol name.
            // func @export("linkage_name") my_function() {}
            if (!expect(Token::STRING)) return nullptr;

            token = peek_token();
            function->linkage_name = token->string;
            next_token();

            if (!expect_and_eat(Token::RIGHT_PAREN)) return nullptr;
        } else if (token->type == Token::TAG_FLAGS) {
            compiler->report_error(token, "@flags tag is not valid for function types.");
            next_token();
        }

        token = peek_token();
    }

    if (is_operator_function) {
        token = peek_token();
        function->operator_type = token->type;

        Ast_Identifier *ident = PARSER_NEW(Ast_Identifier);
        ident->enclosing_scope = get_current_scope();

        if (token->type == Token::LEFT_BRACKET) {
            next_token();

            if (!expect_and_eat(Token::RIGHT_BRACKET)) return nullptr;

            if (peek_token()->type == Token::EQUALS) {
                next_token();
                ident->name = compiler->make_atom(OPERATOR_BRACKET_EQUALS_NAME);
            } else {
                ident->name = compiler->make_atom(OPERATOR_BRACKET_NAME);
            }
        } else {
            if (!is_valid_overloadable_operator(token->type)) {
                String op_name = token_type_to_string(token->type);
                defer { free(op_name.data); };
                compiler->report_error(token, "Token '%.*s' is not a valid operator for overloading.\n", PRINT_ARG(op_name));
                return nullptr;
            } else {
                ident->name = compiler->make_operator_atom(token->type);
                next_token();
            }
        }

        function->identifier = ident;
    } else {
        Ast_Identifier *ident = parse_identifier();
        if (!ident) return nullptr;
        function->identifier = ident;
    }

    token = peek_token();
    if (token->type == Token::LEFT_ANGLE) {
        Ast_Scope *polymorphic_scope = PARSER_NEW(Ast_Scope);
        polymorphic_scope->is_template_argument_block = true;
        polymorphic_scope->parent = get_current_scope();
        function->polymorphic_type_alias_scope = polymorphic_scope;
        next_token();

        push_scopes(polymorphic_scope);

        token = peek_token();
        while (token->type != Token::END) {
            Ast_Type_Alias *alias = PARSER_NEW(Ast_Type_Alias);
            alias->identifier = parse_identifier();

            if (!alias->identifier) {
                compiler->report_error(alias, "Expected identifier in template argument list but got something else.\n");
                return nullptr;
            }

            polymorphic_scope->declarations.add(alias);

            token = peek_token();
            if (token->type == Token::COMMA) {
                next_token();

                token = peek_token();
                continue;
            }

            if (!expect_and_eat(Token::RIGHT_ANGLE)) return nullptr;

            break;
        }

        pop_scopes();

        function->arguments_scope.parent = polymorphic_scope;

        function->is_template_function = true;
    }

    if (!expect_and_eat(Token::LEFT_PAREN)) return nullptr;

    push_scopes(&function->arguments_scope);

    token = peek_token();
    while (token->type != Token::END) {

        if (function->arguments.count > 0 && token->type == Token::COMMA) {
            next_token();
            token = peek_token();
        } else  if (token->type == Token::RIGHT_PAREN) break;

        // @Temporary
        // @Temporary
        // @Temporary
        if (token->type == Token::TEMPORARY_KEYWORD_C_VARARGS) {
            next_token();
            function->is_c_varargs = true;

            token = peek_token();
            if (token->type != Token::RIGHT_PAREN) {
                compiler->report_error(token, "Expected ')' following 'temporary_c_vararg' declarator.\n");
                return nullptr;
            }
            break;
        }

        Ast_Declaration *decl = parse_variable_declaration(false);
        if (decl) {
            decl->is_let = true;
            function->arguments.add(decl);
            add_declaration(&function->arguments_scope.declarations, decl);
        }

        if (compiler->errors_reported) return nullptr;

        token = peek_token();
    }

    pop_scopes();

    if (!expect_and_eat(Token::RIGHT_PAREN)) return nullptr;

    if (peek_token()->type == Token::ARROW) {
        token = peek_token();
        if (!expect_and_eat(Token::ARROW)) return nullptr;

        Ast_Type_Instantiation *type_inst = parse_type_inst();
        if (!type_inst) {
            compiler->report_error(token, "Could not parse type following '->'.\n");
            return nullptr;
        }

        function->return_type = type_inst;

        token = peek_token();
    }


    if (peek_token()->type == '{') {
        Ast_Scope *scope = PARSER_NEW(Ast_Scope);
        scope->parent = &function->arguments_scope;
        parse_scope(scope, true);
        function->scope = scope;
        function->scope->owning_function = function;
    } else {
        if (!expect_and_eat(Token::SEMICOLON)) return nullptr;
    }

    currently_parsing_function = old_function;
    return function;
}
