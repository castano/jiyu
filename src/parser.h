
#ifndef PARSER_H
#define PARSER_H

#include "general.h"
#include "ast.h"

#include "lexer.h" // for Lexer, Token, Token::Type

struct Compiler;

struct Parser {
    Compiler *compiler;
    Lexer *lexer;
    array_count_type current_token = 0;
    
    Array<Ast_Scope *> scope_stack;
    Array<Ast_Scope *> canonical_scope_stack;
    
    Ast_Function *currently_parsing_function = nullptr;
    
    Parser(Lexer *lexer) {
        this->lexer = lexer;
        this->compiler = lexer->compiler;
    }
    
    Token *next_token();
    Token *peek_token();
    
    Ast_Scope *get_current_scope();
    Ast_Scope *get_current_canonical_scope();
    void push_scopes(Ast_Scope *scope);
    void pop_scopes();
    
    bool expect(Token::Type type);
    bool expect_and_eat(Token::Type type);
    
    Ast_Type_Instantiation *wrap_primitive_type(Ast_Type_Info *info);
    
    Ast_Identifier *parse_identifier();
    Ast_Expression *parse_primary_expression();
    Ast_Expression *parse_postfix_expression();
    Ast_Expression *parse_unary_expression();
    Ast_Expression *parse_multiplicative_expression();
    Ast_Expression *parse_additive_expression();
    
    Ast_Expression *parse_shift_expression();
    Ast_Expression *parse_relational_expression();
    Ast_Expression *parse_equality_expression();
    Ast_Expression *parse_and_expression();
    Ast_Expression *parse_exclusive_or_expression();
    Ast_Expression *parse_inclusive_or_expression();
    Ast_Expression *parse_logical_and_expression();
    Ast_Expression *parse_logical_xor_expression();
    Ast_Expression *parse_logical_or_expression();
    
    Ast_Expression *parse_expression();
    Ast_Expression *parse_statement();
    
    Ast_Type_Instantiation *parse_type_inst();
    
    Ast_Declaration *parse_variable_declaration(bool expect_var_keyword, bool enum_value_declaration = false);
    void parse_scope(Ast_Scope *scope, bool requires_braces, bool only_one_statement = false, bool push_scope = true, bool is_for_case = false);
    void parse_enum_scope(Ast_Scope *scope);
    
    Ast_Function *parse_function();

    bool add_declaration(Array<Ast_Expression *> * declarations, Ast_Expression * decl);
};

#endif
