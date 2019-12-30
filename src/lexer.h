
#ifndef LEXER_H
#define LEXER_H

#include "general.h"

struct Token {
    enum Type {
        DOT          = '.',
        COMMA        = ',',
        EQUALS       = '=',
        SEMICOLON    = ';',
        COLON        = ':',
        LEFT_PAREN   = '(',
        RIGHT_PAREN  = ')',
        STAR         = '*',
        SLASH        = '/',
        PERCENT      = '%',
        PLUS         = '+',
        MINUS        = '-',
        VERTICAL_BAR = '|', // bitwise Or
        CARET        = '^', // bitwise Xor
        AMPERSAND    = '&', // bitwise And

        LEFT_ANGLE   = '<',
        RIGHT_ANGLE  = '>',

        END = 256,
        INTEGER,
        FLOAT,
        IDENTIFIER,
        STRING,


        KEYWORD_FUNC,
        KEYWORD_VAR,
        KEYWORD_LET,
        KEYWORD_TYPEALIAS,
        KEYWORD_STRUCT,
        KEYWORD_UNION,
        KEYWORD_ENUM,
        KEYWORD_LIBRARY,
        KEYWORD_FRAMEWORK,

        KEYWORD_IF,
        KEYWORD_ELSE,

        KEYWORD_WHILE,
        KEYWORD_BREAK,
        KEYWORD_CONTINUE,
        KEYWORD_FOR,

        KEYWORD_RETURN,

        KEYWORD_VOID,
        KEYWORD_STRING,
        KEYWORD_INT,
        KEYWORD_UINT,
        KEYWORD_UINT8,
        KEYWORD_UINT16,
        KEYWORD_UINT32,
        KEYWORD_UINT64,
        KEYWORD_INT8,
        KEYWORD_INT16,
        KEYWORD_INT32,
        KEYWORD_INT64,
        KEYWORD_FLOAT,
        KEYWORD_DOUBLE,
        KEYWORD_BOOL,
        KEYWORD_TRUE,
        KEYWORD_FALSE,
        KEYWORD_NULL,

        KEYWORD_CAST,
        KEYWORD_SIZEOF,
        KEYWORD_TYPEOF,
        KEYWORD_STRIDEOF,
        KEYWORD_ALIGNOF,
        
        TAG_C_FUNCTION,
        TAG_META,
        TAG_EXPORT,

        TEMPORARY_KEYWORD_C_VARARGS = 400,

        GE_OP,                // >=
        LE_OP,                // <=
        NE_OP,                // !=
        EQ_OP,                // ==
        AND_OP,               // &&
        XOR_OP,               // ^^
        OR_OP,                // ||
        ARROW,                // ->
        DEREFERENCE_OR_SHIFT, // <<
        RIGHT_SHIFT,          // >>

        PLUS_EQ,              // +=
        MINUS_EQ,             // -=
        STAR_EQ,              // *=
        SLASH_EQ,             // /=
        PERCENT_EQ,           // %=
        AMPERSAND_EQ,         // &=
        VERTICAL_BAR_EQ,      // |=
        CARET_EQ,             // ^=

        DOTDOT,               // ..
        DOTDOTLT,             // ..<

        COMMENT,
    };

    Type type;
    TextSpan text_span;
    String filename;

    String string;
    s64    integer = 0;
    double _float  = 0;

    Token() {}

    Token(Type type, TextSpan span) {
        this->type = type;
        this->text_span = span;
    }
};

struct Compiler;

struct Lexer {
    String filename;

    string_length_type current_char = 0;
    String text;

    Array<Token> tokens;
    Compiler *compiler;

    Lexer(Compiler *compiler, String input_text, String filename) {
        this->text = input_text;
        this->filename = filename;
        this->compiler = compiler;
    }

    Token make_token(Token::Type type, Span span);
    Token make_eof_token();
    Token make_string_token(Token::Type type, Span span, String string);
    Token make_integer_token(s64 value, Span span);
    Token make_float_token(double value, Span span);

    Token lex_string(char delim);
    Token lex_multiline_string();

    void eat_whitespace();
    Token lex_token();
    void tokenize_text();
};

int to_lower(int c);

#endif
