
#include "lexer.h"
#include "compiler.h"


static bool is_whitespace(char c) {
    return c == ' ' || c == '\t' || c == '\v' || c == '\r' || c == '\n';
}

int to_lower(int c) {
    if (c >= 'A' && c <= 'Z') return c + 0x20;

    return c;
}

static bool is_letter(int c) {
    c = to_lower(c);
    return c >= 'a' && c <= 'z';
}

static bool is_digit(int c, int radix = 10) {

    if (radix == 16) {
        int l = to_lower(c);
        if (l >= 'a' && l <= 'f') return true;
    }

    return c >= '0' && c <= '9';
}

// @TODO UTF8 identifier support?
static bool starts_identifier(char c) {
    return c == '_' || is_letter(c);
}

static bool continues_identifier(char c) {
    return starts_identifier(c) || is_digit(c);
}

Token Lexer::make_token(Token::Type type, Span span) {
    Token t = Token(type, TextSpan(text, span));
    t.filename = filename;
    return t;
}

Token Lexer::make_eof_token() {
    return make_token(Token::END, Span(text.length, 0));
}

Token Lexer::make_string_token(Token::Type type, Span span, String string) {
    Token t = make_token(type, span);
    t.string = string;
    return t;
}

Token Lexer::make_integer_token(s64 value, Span span) {
    Token t = make_token(Token::INTEGER, span);
    t.integer = value;
    return t;
}

Token Lexer::make_float_token(double value, Span span) {
    Token t = make_token(Token::FLOAT, span);
    t._float = value;
    return t;
}

void Lexer::eat_whitespace() {
    while (current_char < text.length && is_whitespace(text[current_char])) {
        current_char++;
    }
}

static bool translate_escape_sequence(char c, String & output_string) {
    if (c == '0') {
        output_string.length++;
        output_string.data[output_string.length-1] = '\0';
    }
    else if (c == 'n') {
        output_string.length++;
        output_string.data[output_string.length-1] = '\n';
    }
    else if (c == 'r') {
        output_string.length++;
        output_string.data[output_string.length-1] = '\r';
    }
    else if (c == 't') {
        output_string.length++;
        output_string.data[output_string.length-1] = '\t';
    }
    else if (c == '\\') {
        output_string.length++;
        output_string.data[output_string.length-1] = '\\';
    }
    else if (c == '\"') {
        output_string.length++;
        output_string.data[output_string.length-1] = '\"';
    }
    else if (c == '\'') {
        output_string.length++;
        output_string.data[output_string.length-1] = '\'';
    }
    else {
        return false;
    }
    // @Incomplete Add support for unicode scalar values \u{###}

    return true;
}

Token Lexer::lex_string(char delim) {
    assert(text[current_char] == delim);

    char *type = "string";
    if (delim == '\'') type = "character";

    auto start = current_char;
    current_char++;

    while (current_char < text.length  && text[current_char] != delim) {
        if (text[current_char] == '\n') {
            // create a faux token for reporting
            Token t = make_string_token(Token::STRING, Span(start, current_char - start), text.substring(start, current_char - start));
            compiler->report_error(&t, "Newline found while lexing %s constant!", type);

            // return the token so we dont report other errors related to lexing this string
            return t;
        } else if (text[current_char] == '\\') {
            // @Cleanup do we really have to do this??
            if (current_char + 1 < text.length) {
                current_char ++; // pass these so that we can translate them later in the final string
            }
        }

        current_char++;
    }

    if (current_char < text.length && text[current_char] == delim) {
        current_char++;

        auto length = current_char - start;

        String input = text.substring(start+1, length-2);
        String output_string = copy_string(input);
        output_string.length = 0;

        for (string_length_type i = 0; i < input.length; ++i) {
            if (input[i] == '\\') {
                if (i + 1 < input.length) {
                    if (translate_escape_sequence(input[i + 1], output_string)) {
                        ++i;
                        continue;
                    }
                    else {
                        Token t = make_string_token(Token::STRING, Span(i, i+1), text.substring(i, i+1));
                        compiler->report_error(&t, "Unrecognized escape sequence.");
                        return t;
                    }
                }
            }

            output_string.length++;
            output_string.data[output_string.length-1] = input[i];
        }

        return make_string_token(Token::STRING, Span(start, length), output_string);
    } else if (current_char >= text.length) {
        // create a faux token for reporting
        Token t = make_string_token(Token::STRING, Span(start, current_char - start), text.substring(start, current_char - start));
        compiler->report_error(&t, "End-of-file found while lexing %s constant!", type);

        // return the token so we dont report other errors related to lexing this string
        return t;
    } else {
        assert(false);

        // Dummy token, we should not get here normally.
        Token t;
        return t;
    }
}

Token Lexer::lex_multiline_string() {
    assert(strncmp(text.data+current_char, "\"\"\"", 3) == 0);

    auto start = current_char;
    current_char += 4;

    // Find end of the string.
    while (current_char < text.length && (text[current_char] != '\"' || text[current_char - 1] != '\"' || text[current_char - 2] != '\"')) {
        current_char++;
    }

    if (current_char >= text.length) {
        // create a faux token for reporting
        Token t = make_string_token(Token::STRING, Span(start, current_char - start), text.substring(start, current_char - start));
        compiler->report_error(&t, "End-of-file found while lexing multi-line string constant!");

        // return the token so we dont report other errors related to lexing this string
        return t;
    }

    current_char++;
    auto length = current_char - start;

    // Skip spaces at the beginning of the string.
    auto begin = start + 3;
    while (text[begin] == ' ' || text[begin] == '\t' || text[begin] == '\r') {
        begin += 1;
    }
    if (text[begin] != '\n') {
        // If not an empty line, then do not skip.
        begin = start + 3;
    }
    else {
        begin += 1;
    }

    // Skip whitespace at the end of the string.
    auto end = current_char - 3 - 1;
    while (text[end] == ' ' || text[end] == '\t' || text[end] == '\r') {
        end -= 1;
    }
    if (text[end] != '\n') {
        // If not an empty line, then do not skip.
        end = current_char - 3 - 1;
    }
    else {
        end -= 1;
    }

    String input = text.substring(begin, end-begin+1);
    String output_string = copy_string(input);  // This is just to allocate the string, we don't need the copy.
    output_string.length = 0;

    if (input.length == 0) {
        return make_string_token(Token::STRING, Span(start, length), output_string);
    }

    // We remove indentation from the beginning of the string, but we don't allow mixing tabs and spaces.
    int indent_spaces = 0;
    int indent_tabs = 0;
    if (input[0] == ' ') {
        while (indent_spaces < input.length && input[indent_spaces] == ' ') indent_spaces += 1;
    }
    else if (input[0] == '\t') {
        while (indent_tabs < input.length && input[indent_tabs] == '\t') indent_tabs += 1;
    }

    for (string_length_type i = indent_spaces + indent_tabs; i < input.length; ++i) {
        {
            output_string.length++;
            output_string.data[output_string.length-1] = input[i];

            // Try to remove indentation.
            if (input[i] == '\n') {
                while (indent_spaces && i+1 < input.length && input[i+1] == ' ') {
                    indent_spaces -= 1; i += 1;
                }
                while (indent_tabs && i+1 < input.length && input[i+1] == '\t') {
                    indent_tabs -= 1; i += 1;
                }
                if (indent_tabs || indent_spaces) {
                    // @@ Warn if indentation not removed?
                }
            }
        }
    }

    return make_string_token(Token::STRING, Span(start, length), output_string);
}


Token Lexer::lex_token() {
    eat_whitespace();

    if (!(current_char < text.length)) return make_eof_token();

    if (starts_identifier(text[current_char]) || text[current_char] == '@') {
        auto start = current_char;
        current_char++;

        while (current_char < text.length && continues_identifier(text[current_char])) {
            current_char++;
        }

        string_length_type length = current_char - start;
        Token result = make_string_token(Token::IDENTIFIER, Span(start, length), text.substring(start, length));

        // @Cleanup find a faster way to implement these things
        if      (result.string == to_string("func"))      result.type = Token::KEYWORD_FUNC;
        else if (result.string == to_string("var"))       result.type = Token::KEYWORD_VAR;
        else if (result.string == to_string("let"))       result.type = Token::KEYWORD_LET;
        else if (result.string == to_string("typealias")) result.type = Token::KEYWORD_TYPEALIAS;
        else if (result.string == to_string("struct"))    result.type = Token::KEYWORD_STRUCT;
        else if (result.string == to_string("union"))     result.type = Token::KEYWORD_UNION;
        else if (result.string == to_string("library"))   result.type = Token::KEYWORD_LIBRARY;
        else if (result.string == to_string("framework")) result.type = Token::KEYWORD_FRAMEWORK;

        else if (result.string == to_string("void"))   result.type = Token::KEYWORD_VOID;
        else if (result.string == to_string("string")) result.type = Token::KEYWORD_STRING;

        else if (result.string == to_string("int"))    result.type = Token::KEYWORD_INT;
        else if (result.string == to_string("uint"))   result.type = Token::KEYWORD_UINT;

        else if (result.string == to_string("uint8"))  result.type = Token::KEYWORD_UINT8;
        else if (result.string == to_string("uint16")) result.type = Token::KEYWORD_UINT16;
        else if (result.string == to_string("uint32")) result.type = Token::KEYWORD_UINT32;
        else if (result.string == to_string("uint64")) result.type = Token::KEYWORD_UINT64;
        else if (result.string == to_string("int8"))   result.type = Token::KEYWORD_INT8;
        else if (result.string == to_string("int16"))  result.type = Token::KEYWORD_INT16;
        else if (result.string == to_string("int32"))  result.type = Token::KEYWORD_INT32;
        else if (result.string == to_string("int64"))  result.type = Token::KEYWORD_INT64;
        else if (result.string == to_string("float"))  result.type = Token::KEYWORD_FLOAT;
        else if (result.string == to_string("double")) result.type = Token::KEYWORD_DOUBLE;
        else if (result.string == to_string("bool"))   result.type = Token::KEYWORD_BOOL;
        else if (result.string == to_string("true"))   result.type = Token::KEYWORD_TRUE;
        else if (result.string == to_string("false"))  result.type = Token::KEYWORD_FALSE;
        else if (result.string == to_string("null"))   result.type = Token::KEYWORD_NULL;
        else if (result.string == to_string("if"))     result.type = Token::KEYWORD_IF;
        else if (result.string == to_string("else"))   result.type = Token::KEYWORD_ELSE;
        else if (result.string == to_string("while"))  result.type = Token::KEYWORD_WHILE;
        else if (result.string == to_string("break"))  result.type = Token::KEYWORD_BREAK;
        else if (result.string == to_string("continue")) result.type = Token::KEYWORD_CONTINUE;
        else if (result.string == to_string("for"))    result.type = Token::KEYWORD_FOR;

        else if (result.string == to_string("return")) result.type = Token::KEYWORD_RETURN;

        else if (result.string == to_string("cast"))     result.type = Token::KEYWORD_CAST;
        else if (result.string == to_string("sizeof"))   result.type = Token::KEYWORD_SIZEOF;
        else if (result.string == to_string("strideof")) result.type = Token::KEYWORD_STRIDEOF;
        else if (result.string == to_string("alignof"))  result.type = Token::KEYWORD_ALIGNOF;

        // @Cleanup we should probably have a "tag" token
        else if (result.string == to_string("@c_function"))  result.type = Token::TAG_C_FUNCTION;
        else if (result.string == to_string("@metaprogram")) result.type = Token::TAG_META;
        else if (result.string == to_string("@export")) result.type = Token::TAG_EXPORT;

        else if (result.string == to_string("temporary_c_vararg")) result.type = Token::TEMPORARY_KEYWORD_C_VARARGS;

        return result;
    } else if (is_digit(text[current_char])) {
        auto start = current_char;
        auto number_start = current_char;

        int radix = 10;
        bool is_float = false;
        if (text[current_char] == '0') {
            current_char++;

            if (current_char < text.length &&
                (text[current_char] == 'x' || text[current_char] == 'X')) {
                radix = 16;
                current_char++;
                number_start = current_char;
            }
        }

        while (current_char < text.length && (is_digit(text[current_char], radix) || text[current_char] == '.')) {
            if (text[current_char] == '.') {
                if (radix == 10) {
                    if (current_char+1 < text.length && text[current_char+1] == '.') {
                        // if we encounter the .. operator then bail out.
                        break;
                    } else if (current_char+1 < text.length && is_digit(text[current_char+1], radix)) {
                        is_float = true;
                    } else {
                        break;
                    }
                } else {
                    // floats must be radix 10
                    break;
                }
            }

            current_char++;
        }

        char *value_string = compiler->get_temp_c_string(text.substring(number_start, current_char - number_start));

        if (is_float) {
            auto value = strtod(value_string, nullptr);

            return make_float_token(value, Span(start, current_char - start));
        } else {
            auto value = strtoull(value_string, nullptr, radix);

            return make_integer_token(value, Span(start, current_char - start));
        }
    } else if (text[current_char] == '\"') {
        if (current_char+2 < text.length && text[current_char+1] == '\"' && text[current_char+2] == '\"') {
            Token value = lex_multiline_string();
            return value;
        }
        else {
            Token value = lex_string('\"');
            return value;
        }
    } else if (text[current_char] == '\'') {
        Token value = lex_string('\'');
        if (compiler->errors_reported) return value;

        String st = value.string;
        // @Temporary should be 8
        if (st.length > 4) {
            compiler->report_error(&value, "Character constant too large to fit in an integer value!\n");
            return value;
        }

        Token out;
        out.type = Token::INTEGER;
        out.text_span = value.text_span;
        out.filename = value.filename;

        for (string_length_type i = 0; i < st.length; ++i) {
            out.integer = out.integer | (((u64)st[i] & 0xFF) << i*8);
        }

        return out;
    } else if (text[current_char] == '-') {
        if (current_char+1 < text.length && text[current_char+1] == '>') {
            string_length_type start = current_char;
            current_char += 2;
            return make_token(Token::ARROW, Span(start, 2));
        }

        if (current_char+1 < text.length && text[current_char+1] == '=') {
            string_length_type start = current_char;
            current_char += 2;
            return make_token(Token::MINUS_EQ, Span(start, 2));
        }
    } else if (text[current_char] == '.') {
        if (current_char+1 < text.length && text[current_char+1] == '.') {
            if (current_char+2 < text.length && text[current_char+2] == '<') {
                current_char += 3;
                return make_token(Token::DOTDOTLT, Span(current_char, 3));
            }
            else {
                current_char += 2;
                return make_token(Token::DOTDOT, Span(current_char, 2));
            }
        }
    } else if (text[current_char] == '/') {
        if (current_char+1 < text.length && text[current_char+1] == '*') {
            string_length_type start = current_char;
            current_char += 2;
            s64 stack = 1;
            while (current_char < text.length && stack > 0) {
                if (text[current_char] == '*') {
                    if (current_char+1 < text.length && text[current_char+1] == '/') {
                        stack--;
                        current_char += 2;
                        continue;
                    }
                }

                if (text[current_char] == '/') {
                    if (current_char+1 < text.length && text[current_char+1] == '*') {
                        stack++;
                        current_char += 2;
                        continue;
                    }
                }

                current_char++;
            }

            string_length_type length = current_char - start;
            // :CommentTokens:
            // Returning a token here because we dont return pointers to tokens
            // and if we returned a recursive lex_token() call, we can get defeated quite quickly from
            // people spamming comment-after-comment.

            Token result = make_string_token(Token::COMMENT, Span(start, length), text.substring(start, length));
            return result;
        } else if (current_char+1 < text.length && text[current_char+1] == '/') {
            string_length_type start = current_char;
            current_char += 2;

            while (current_char < text.length && text[current_char] != '\n') current_char++;

            string_length_type length = current_char - start;

            // :CommentTokens:
            Token result = make_string_token(Token::COMMENT, Span(start, length), text.substring(start, length));
            return result;
        }
    } else if (text[current_char] == '<') {
        if (current_char+1 < text.length && text[current_char+1] == '<') {
            string_length_type start = current_char;
            current_char += 2;
            return make_token(Token::DEREFERENCE_OR_SHIFT, Span(start, 2));
        } else if (current_char+1 < text.length && text[current_char+1] == '=') {
            string_length_type start = current_char;
            current_char += 2;
            return make_token(Token::LE_OP, Span(start, 2));
        }
    } else if (text[current_char] == '>') {
        if (current_char+1 < text.length && text[current_char+1] == '>') {
            string_length_type start = current_char;
            current_char += 2;
            return make_token(Token::RIGHT_SHIFT, Span(start, 2));
        } else if (current_char+1 < text.length && text[current_char+1] == '=') {
            string_length_type start = current_char;
            current_char += 2;
            return make_token(Token::GE_OP, Span(start, 2));
        }
    } else if (text[current_char] == '=') {
        if (current_char+1 < text.length && text[current_char+1] == '=') {
            string_length_type start = current_char;
            current_char += 2;
            return make_token(Token::EQ_OP, Span(start, 2));
        }
    } else if (text[current_char] == '!') {
        if (current_char+1 < text.length && text[current_char+1] == '=') {
            string_length_type start = current_char;
            current_char += 2;
            return make_token(Token::NE_OP, Span(start, 2));
        }
    } else if (text[current_char] == '&') {
        if (current_char+1 < text.length && text[current_char+1] == '&') {
            string_length_type start = current_char;
            current_char += 2;
            return make_token(Token::AND_OP, Span(start, 2));
        }

        if (current_char+1 < text.length && text[current_char+1] == '=') {
            string_length_type start = current_char;
            current_char += 2;
            return make_token(Token::AMPERSAND_EQ, Span(start, 2));
        }
    } else if (text[current_char] == '^') {
        if (current_char+1 < text.length && text[current_char+1] == '^') {
            string_length_type start = current_char;
            current_char += 2;
            return make_token(Token::XOR_OP, Span(start, 2));
        }

        if (current_char+1 < text.length && text[current_char+1] == '=') {
            string_length_type start = current_char;
            current_char += 2;
            return make_token(Token::CARET_EQ, Span(start, 2));
        }
    } else if (text[current_char] == '|') {
        if (current_char+1 < text.length && text[current_char+1] == '|') {
            string_length_type start = current_char;
            current_char += 2;
            return make_token(Token::OR_OP, Span(start, 2));
        }

        if (current_char+1 < text.length && text[current_char+1] == '=') {
            string_length_type start = current_char;
            current_char += 2;
            return make_token(Token::VERTICAL_BAR_EQ, Span(start, 2));
        }
    } else if (text[current_char] == '+') {
        if (current_char+1 < text.length && text[current_char+1] == '=') {
            string_length_type start = current_char;
            current_char += 2;
            return make_token(Token::PLUS_EQ, Span(start, 2));
        }
    } else if (text[current_char] == '*') {
        if (current_char+1 < text.length && text[current_char+1] == '=') {
            string_length_type start = current_char;
            current_char += 2;
            return make_token(Token::STAR_EQ, Span(start, 2));
        }
    } else if (text[current_char] == '/') {
        if (current_char+1 < text.length && text[current_char+1] == '=') {
            string_length_type start = current_char;
            current_char += 2;
            return make_token(Token::SLASH_EQ, Span(start, 2));
        }
    } else if (text[current_char] == '%') {
        if (current_char+1 < text.length && text[current_char+1] == '=') {
            string_length_type start = current_char;
            current_char += 2;
            return make_token(Token::PERCENT_EQ, Span(start, 2));
        }
    }

    char c = text[current_char];
    auto start = current_char++;
    return make_token((Token::Type)c, Span(start, 1));
}

void Lexer::tokenize_text() {
    Token tok;
    do {
        tok = lex_token();

        // Ignore Token::COMMENT since in most cases we dont care about these
        if (tok.type == Token::COMMENT) continue;

        tokens.add(tok);
    } while (tok.type != Token::END);
}
