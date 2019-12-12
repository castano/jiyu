
#include "general.h"
#include "compiler.h"
#include "lexer.h"
#include "parser.h"
#include "sema.h"
#include "llvm.h"
#include "os_support.h"
#include "clang_import.h"

#ifdef WIN32
#pragma warning(push, 0)
#endif

#include "llvm/Target/TargetMachine.h"

#ifdef WIN32
#pragma warning(pop)
#endif

#include <stdio.h> // for vprintf
#include <new> // for placement new

#define COMPILER_NEW(type)  (new (this->get_memory(sizeof(type))) type())
// sigh, c++
#define COMPILER_NEW2(type) (new (compiler->get_memory(sizeof(type))) type())

bool types_match(Ast_Type_Info *left, Ast_Type_Info *right) {
    left  = get_final_type(left);
    right = get_final_type(right);

    if (left->type != right->type) return false;
    if (left->size != right->size) return false;

    if (left->type == Ast_Type_Info::INTEGER) {
        return left->is_signed == right->is_signed;
    }

    if (left->type == Ast_Type_Info::POINTER) {
        assert(left->pointer_to && right->pointer_to);
        return types_match(left->pointer_to, right->pointer_to);
    }

    if (left->type == Ast_Type_Info::ARRAY) {
        return types_match(left->array_element, right->array_element) &&
            left->array_element_count == right->array_element_count &&
            left->is_dynamic == right->is_dynamic;
    }

    if (left->type == Ast_Type_Info::STRUCT) {
        // @Incomplete how would this work for anonymous structs for which a struct declaration doesnt exist? Do we always just create a faux declaration in that case?
        assert(left->struct_decl);
        assert(right->struct_decl);
        return left->struct_decl == right->struct_decl;
    }

    if (left->type == Ast_Type_Info::FUNCTION) {
        if (left->is_c_function != right->is_c_function) return false;
        if (left->is_c_varargs  != right->is_c_varargs) return false;
        if (!types_match(left->return_type, right->return_type)) return false;

        if (left->arguments.count != right->arguments.count) return false;

        for (array_count_type i = 0; i < left->arguments.count; ++i) {
            auto larg = left->arguments[i];
            auto rarg = right->arguments[i];

            if (!types_match(larg, rarg)) return false;
        }

        return true;
    }

    return true;
}

void *Compiler::get_memory(array_count_type amount) {
    return memory_pool.allocate(amount);
}

String Compiler::copy_string(String s) {
    String out;
    out.length = s.length;

    auto length = s.length;
    if (s.data && s.length) {
        out.data = (char *)this->get_memory(length);
        memcpy(out.data, s.data, length);
    }
    return out;
}

Ast_Type_Info *Compiler::make_array_type(Ast_Type_Info *element, array_count_type count, bool is_dynamic) {
    Ast_Type_Info *info = COMPILER_NEW(Ast_Type_Info);
    info->type = Ast_Type_Info::ARRAY;
    info->array_element       = element;
    info->array_element_count = count;
    info->is_dynamic = is_dynamic;

    if (count >= 0) {
        auto element_final_type = get_final_type(element);

        assert(is_dynamic == false);
        info->size      = element_final_type->size * count;
        info->alignment = element_final_type->alignment;
        info->stride = info->size;
    } else {
        if (!is_dynamic) {
            info->size = this->pointer_size * 2;
        } else {
            info->size = this->pointer_size * 3;
        }

        info->alignment = this->pointer_size; // @TargetInfo @PointerSize
        info->stride = info->size;
    }

    assert(info->alignment >= 0);

    add_to_type_table(info);
    return info;
}

Ast_Type_Info *Compiler::make_pointer_type(Ast_Type_Info *pointee) {
    Ast_Type_Info *info = COMPILER_NEW(Ast_Type_Info);
    info->type = Ast_Type_Info::POINTER;
    info->pointer_to = pointee;
    info->size = this->pointer_size; // @TargetInfo
    info->alignment = info->size;
    info->stride    = info->size;

    add_to_type_table(info);
    return info;
}

#define COPY(name) do { info->name = aliasee->name; } while(0)

Ast_Type_Info *Compiler::make_type_alias(Ast_Type_Info *aliasee) {
    Ast_Type_Info *info = COMPILER_NEW(Ast_Type_Info);
    info->type = Ast_Type_Info::ALIAS;
    info->alias_of  = aliasee;

    // Dont copy these, if you want to check this info
    // you should call get_final_type.
    // COPY(size);
    // COPY(alignment);
    // COPY(stride);

    // Copy important things from source=>alias. Most of the typechecking code
    // shouldnt and doesnt care if something is a typealias, so one should be
    // able to do this without going through alias_of:
    /*
    if (is_pointer_type(alias)) {
        auto info = alias->pointer_to;
    }
    */
    // This should also work if aliasee is an alias because this data would propagate back
    // to this new alias, but maybe that isnt true! @TODO

    // Dont copy alias_decl or alias_of here.
    COPY(is_signed);
    COPY(pointer_to);
    COPY(array_element);

    COPY(array_element_count);
    COPY(is_dynamic);

    COPY(struct_decl);

    for (auto mem: aliasee->struct_members) info->struct_members.add(mem);

    for (auto arg: aliasee->arguments) info->arguments.add(arg);
    COPY(return_type);
    COPY(is_c_function);
    COPY(is_c_varargs);

    add_to_type_table(info);
    return info;
}

Ast_Type_Info *make_struct_type(Compiler *compiler, Ast_Struct *_struct) {
    Ast_Type_Info *info = COMPILER_NEW2(Ast_Type_Info);
    info->type = Ast_Type_Info::STRUCT;
    info->struct_decl = _struct;
    info->is_union = _struct->is_union;
    return info;
}

Ast_Type_Info *Compiler::make_function_type(Ast_Function *function) {
    Ast_Type_Info *info = COMPILER_NEW(Ast_Type_Info);
    info->type      = Ast_Type_Info::FUNCTION;
    info->size      = this->type_ptr_void->size;
    info->stride    = this->type_ptr_void->stride;
    info->alignment = this->type_ptr_void->alignment;

    info->is_c_function = function->is_c_function;
    info->is_c_varargs  = function->is_c_varargs;

    for (auto arg: function->arguments) {
        assert(get_type_info(arg));

        auto arg_info = get_type_info(arg);

        info->arguments.add(arg_info);
    }

    if (function->return_decl) {
        assert(function->return_decl);
        info->return_type = get_type_info(function->return_decl);
        assert(info->return_type);
    } else {
        info->return_type = this->type_void;
    }

    add_to_type_table(info);
    return info;
}

static Ast_Type_Info *make_int_type(Compiler *compiler, bool is_signed, s64 size) {
    Ast_Type_Info *info = COMPILER_NEW2(Ast_Type_Info);
    info->type = Ast_Type_Info::INTEGER;
    info->is_signed = is_signed;
    info->size = size;
    info->alignment = info->size;
    info->stride = info->size;

    compiler->add_to_type_table(info);
    return info;
}

static Ast_Type_Info *make_float_type(Compiler *compiler, s64 size) {
    Ast_Type_Info *info = COMPILER_NEW2(Ast_Type_Info);
    info->type = Ast_Type_Info::FLOAT;
    info->size = size;
    info->alignment = info->size;
    info->stride    = info->size;

    compiler->add_to_type_table(info);
    return info;
}

char *Compiler::get_temp_c_string(String s) {
    char *mem = (char *)malloc(s.length + 1); // @Leak

    memcpy(mem, s.data, s.length);
    mem[s.length] = 0;
    return mem;
}

void Compiler::init() {
    auto target_machine = llvm_gen->TargetMachine;

    pointer_size = target_machine->getPointerSize(0);

    if (this->build_options.verbose_diagnostics) {
        // @TODO compiler->print_diagnostic
        printf("w%d: Target machine pointer size: %d\n", this->instance_number, pointer_size);
    }


    bool is_64bits = false;
    bool is_32bits = false;
    if (pointer_size == 4) {
        is_64bits = false;
        is_32bits = true;
    } else if (pointer_size == 8) {
        is_64bits = true;
        is_32bits = false;
    } else {
        assert(false);
        // @TODO Not sure what the right thing to do would be on a 128-bit machine
        // or a 16 or 8-bit machine..
    }

    type_void = COMPILER_NEW(Ast_Type_Info);
    type_void->type = Ast_Type_Info::VOID;
    add_to_type_table(type_void);

    type_bool = COMPILER_NEW(Ast_Type_Info);
    type_bool->type = Ast_Type_Info::BOOL;
    type_bool->size   = 1;
    type_bool->stride = 1;
    type_bool->alignment = 1;
    add_to_type_table(type_bool);

    type_int8  = make_int_type(this, true, 1);
    type_int16 = make_int_type(this, true, 2);
    type_int32 = make_int_type(this, true, 4);
    type_int64 = make_int_type(this, true, 8);

    type_uint8  = make_int_type(this, false, 1);
    type_uint16 = make_int_type(this, false, 2);
    type_uint32 = make_int_type(this, false, 4);
    type_uint64 = make_int_type(this, false, 8);

    type_float32 = make_float_type(this, 4);
    type_float64 = make_float_type(this, 8);

    type_string_data = make_pointer_type(type_uint8);

    if (is_64bits) {
        type_string_length = type_int64; // @TargetInfo
    } else if (is_32bits) {
        type_string_length = type_int32; // @TargetInfo
    }

    type_string = COMPILER_NEW(Ast_Type_Info);
    type_string->type = Ast_Type_Info::STRING;
    type_string->size = type_string_length->size + type_string_data->size;
    type_string->stride = type_string->size;
    type_string->alignment = type_string_length->alignment;
    add_to_type_table(type_string);

    if (is_64bits) {
        type_array_count = type_int64; // @TargetInfo
    } else if (is_32bits) {
        type_array_count = type_int32; // @TargetInfo
    }

    type_info_type = COMPILER_NEW(Ast_Type_Info);
    type_info_type->type = Ast_Type_Info::TYPE;
    type_info_type->size = this->pointer_size;
    type_info_type->stride = type_info_type->size;
    type_info_type->alignment = type_info_type->size;
    add_to_type_table(type_info_type);

    type_ptr_void = make_pointer_type(type_void);

    add_to_type_table(type_ptr_void);

    atom_data      = make_atom(to_string("data"));
    atom_length    = make_atom(to_string("length"));
    atom_count     = make_atom(to_string("count"));
    atom_allocated = make_atom(to_string("allocated"));
    atom_it        = make_atom(to_string("it"));
    atom_it_index  = make_atom(to_string("it_index"));
    atom_main      = make_atom(to_string("main"));
    atom___strings_match = make_atom(to_string("__strings_match"));
    atom_os        = make_atom(to_string("os"));
    atom_MacOSX    = make_atom(to_string("MacOSX"));
    atom_Windows   = make_atom(to_string("Windows"));
    atom_Linux     = make_atom(to_string("Linux"));
}

void Compiler::add_to_type_table(Ast_Type_Info *info) {
    if (info->type_table_index >= 0) return;

    for (auto entry: type_table) {
        if (types_match(entry, info)) {
            info->type_table_index = entry->type_table_index;
            return;
        }
    }

    info->type_table_index = type_table.count;
    type_table.add(info);
}

void Compiler::queue_directive(Ast_Directive *directive) {
    assert(directive->type == AST_DIRECTIVE_LOAD   || directive->type == AST_DIRECTIVE_STATIC_IF
        || directive->type == AST_DIRECTIVE_IMPORT || directive->type == AST_DIRECTIVE_CLANG_IMPORT);

    directive_queue.add(directive);
}

void Compiler::resolve_directives() {
    // Use ordered_remove here because if we handle these out-of-order in which they were
    // queued up, then directives that depend on static_if may resolve before the outer static_if does.
    // All-in-all, I'm not sure if this system is as robust as I'd like and this may need to change,
    // perhaps to a top-down tree-descent system.

    // Spin on the queue length since directives can cause more directives to be added in
    while (directive_queue.count) {
        auto directive = directive_queue[0];
        auto scope_i_belong_to = directive->scope_i_belong_to;
        assert(scope_i_belong_to);
        bool rejected = false;
        while (scope_i_belong_to) {
            if (scope_i_belong_to->rejected_by_static_if) {
                rejected = true;
                break;
            }

            scope_i_belong_to = scope_i_belong_to->parent;
        }

        if (rejected) {
            directive_queue.ordered_remove(0);
            continue;
        }

        if (directive->type == AST_DIRECTIVE_LOAD) {
            auto load = static_cast<Ast_Directive_Load *>(directive);

            // auto name = load->target_filename;
            // printf("%d DEBUG: load '%.*s', rejected? : %s\n", this->instance_number, name.length, name.data, rejected ? "true" : "false");

            void perform_load(Compiler *compiler, Ast *ast, String filename, Ast_Scope *target_scope);
            perform_load(this, load, load->target_filename, load->target_scope);

            if (this->errors_reported) return;

            directive_queue.ordered_remove(0);
        } else if (directive->type == AST_DIRECTIVE_IMPORT) {
            // @Incomplete we need a way to stop imports into a module scope from leaking into the global scope lookup
            // Actually, doesn't this already do that? If we import Basic right now, LibC isnt exposed to the application
            // unless the application also imports LibC. -josh 30 November 2019
            auto import = static_cast<Ast_Directive_Import *>(directive);

            auto name = import->target_filename;

            bool success = false;
            for (auto &module_path : this->module_search_paths) {
                String fullpath = mprintf("%.*s/%.*s.jyu", PRINT_ARG(module_path), PRINT_ARG(name));

                if (file_exists(fullpath)) {
                    name = fullpath; // @Leak
                    success = true;
                    break;
                }
            }

            if (!success) {
                this->report_error(import, "Could not find a module named %.*s.\n", PRINT_ARG(name));
                return;
            }

            import->target_filename = name;

            success = false;
            for (auto im : this->loaded_imports) {
                // @Incomplete target_filename should be canonicalized
                if (im->target_filename == name) {
                    import->imported_scope = im->imported_scope;
                    success = true;
                    break;
                }
            }

            if (!success) {
                Ast_Scope *scope = COMPILER_NEW(Ast_Scope);
                scope->parent = this->preload_scope;
                import->imported_scope = scope;

                // printf("%d DEBUG: import '%.*s'\n", this->instance_number, name.length, name.data);

                void perform_load(Compiler *compiler, Ast *ast, String filename, Ast_Scope *target_scope);
                perform_load(this, import, import->target_filename, import->imported_scope);

                this->loaded_imports.add(import);
            }

            Ast_Scope_Expansion *exp = COMPILER_NEW(Ast_Scope_Expansion);
            exp->text_span = import->imported_scope->text_span;
            exp->filename = import->imported_scope->filename;

            import->scope_i_belong_to->private_declarations.add(exp);

            exp->scope = import->imported_scope;
            exp->expanded_via_import_directive = import;
            import->substitution = exp;

            if (this->errors_reported) return;

            directive_queue.ordered_remove(0);
        } else if (directive->type == AST_DIRECTIVE_STATIC_IF) {
            auto _if = static_cast<Ast_Directive_Static_If *>(directive);
            if (_if->then_scope) _if->then_scope->rejected_by_static_if = true;
            if (_if->else_scope) _if->else_scope->rejected_by_static_if = true;

            sema->typecheck_expression(_if->condition);
            if (this->errors_reported) return;

            auto lit = resolves_to_literal_value(_if->condition);

            if (!lit) {
                this->report_error(_if->condition, "#if condition must be a literal expression.\n");
                return;
            }

            assert(get_type_info(lit));

            Ast_Scope *chosen_block = nullptr;

            switch(lit->literal_type) {
                case Ast_Literal::INTEGER: {
                    if (lit->integer_value != 0) {
                        chosen_block = _if->then_scope;
                    } else {
                        chosen_block = _if->else_scope;
                    }
                    break;
                }
                case Ast_Literal::STRING: {
                    if (lit->string_value != to_string("")) {
                        chosen_block = _if->then_scope;
                    } else {
                        chosen_block = _if->else_scope;
                    }
                    break;
                }

                case Ast_Literal::FLOAT: {
                    if (lit->float_value != 0) {
                        chosen_block = _if->then_scope;
                    } else {
                        chosen_block = _if->else_scope;
                    }
                    break;
                }
                case Ast_Literal::BOOL: {
                    if (lit->bool_value) {
                        chosen_block = _if->then_scope;
                    } else {
                        chosen_block = _if->else_scope;
                    }
                    break;
                }
                case Ast_Literal::NULLPTR: {
                    chosen_block = _if->else_scope;
                    break;
                }
            }

            if (chosen_block) {
                chosen_block->rejected_by_static_if = false;

                Ast_Scope_Expansion *exp = COMPILER_NEW(Ast_Scope_Expansion);
                exp->text_span = chosen_block->text_span;
                exp->filename = chosen_block->filename;

                _if->scope_i_belong_to->declarations.add(exp);

                exp->scope = chosen_block;
                _if->substitution = exp;
            }

            directive_queue.ordered_remove(0);
        } else if (directive->type == AST_DIRECTIVE_CLANG_IMPORT) {
            auto import = static_cast<Ast_Directive_Clang_Import *>(directive);

            // sigh, for some reason, you cannot just pass a string to clang and get
            // an AST back. The code has to exist in a file at some point.

            String path = mprintf(".w%d_temp_c_file.c", this->instance_number);

            bool write_entire_file(String filepath, String data);
            write_entire_file(path, import->string_to_compile);

            char *c_path = to_c_string(path);
            perform_clang_import(this, c_path, import->scope_i_belong_to);

            free(c_path);
            free(path.data);

            if (this->errors_reported) return;

            directive_queue.ordered_remove(0);
        } else {
            assert(false);
        }
    }
}

Atom *Compiler::make_atom(String name) {
    Atom *atom = atom_table->find_atom(name);
    if (!atom) {
        atom = COMPILER_NEW(Atom);
        atom->name = this->copy_string(name);
        atom->hash = atom_table->hash_key(name);

        atom_table->data.add(atom);
    }

    return atom;
}

#define TTY_RED    "\033[0;31m"
#define TTY_RESET  "\033[0m"

void Compiler::report_diagnostic_valist(String filename, String source, Span error_location, char *level_name, char *fmt, va_list args) {

    string_length_type l0 = -1;
    string_length_type c0 = -1;

    string_length_type l1 = -1;
    string_length_type c1 = -1;

    error_location.map_to_text_coordinates(source, &l0, &c0, &l1, &c1);

    // @Cleanup these static_casts by using the right printf format spec
    printf("w%lld:%.*s:%d,%d: %s: ", this->instance_number, PRINT_ARG(filename), static_cast<int>(l0), static_cast<int>(c0), level_name);
    vprintf(fmt, args);
    printf("\n");

    string_length_type start_char = -1;
    string_length_type end_char   = -1;
    string_length_type num_lines  = -1;
    error_location.get_surrounding_lines(source, 1, &start_char, &end_char, &num_lines);

    assert(start_char >= 0 && end_char >= 0);
    assert(end_char <= source.length);
    assert(start_char <= end_char);
    // assert(num_lines >= 0);

    // printf("start char: %d\n", start_char);
    // printf("end   char: %d\n", end_char);
    // printf("Span: %d, %d\n", error_location.start, error_location.start + error_location.length - 1);

    String s;
    s.data = source.data + start_char;
    s.length = end_char - start_char;

    string_length_type char_current = start_char;
    for (string_length_type i = 0; i < num_lines; ++i) {
        // String temp = s;

        printf(">    ");

        while (s.length > 0 && s[0] != '\n') {
            // printf("char_current: %d\n", char_current);
            if (char_current == error_location.start) {
                printf(TTY_RED);
            } else if (char_current == (error_location.start + error_location.length)) {
                printf(TTY_RESET);
            }

            putchar(s[0]);
            advance(&s);
            char_current++;
        }

        char_current++; // count newline character
        if (char_current == (error_location.start + error_location.length)) {
            printf(TTY_RESET);
        }

        advance(&s);

        putchar('\n');
    }

    putchar('\n');
}

void Compiler::report_error(Token *tok, char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    String filename;
    String source;
    Span span;

    if (tok) {
        filename = tok->filename;
        source = tok->text_span.string;
        span = tok->text_span.span;
    }

    report_diagnostic_valist(filename, source, span, "error", fmt, args);
    va_end(args);

    errors_reported += 1;
}


void Compiler::report_error(Ast *ast, char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    String filename;
    String source;
    Span span;

    if (ast) {
        filename = ast->filename;
        source = ast->text_span.string;
        span = ast->text_span.span;
    }

    report_diagnostic_valist(filename, source, span, "error", fmt, args);
    va_end(args);

    errors_reported += 1;
}

void Compiler::report_warning(Token *tok, char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    String filename;
    String source;
    Span span;

    if (tok) {
        filename = tok->filename;
        source = tok->text_span.string;
        span = tok->text_span.span;
    }

    report_diagnostic_valist(filename, source, span, "warning", fmt, args);
    va_end(args);

    // __builtin_debugtrap();
}


void Compiler::report_warning(Ast *ast, char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    String filename;
    String source;
    Span span;

    if (ast) {
        filename = ast->filename;
        source = ast->text_span.string;
        span = ast->text_span.span;
    }

    report_diagnostic_valist(filename, source, span, "warning", fmt, args);
    va_end(args);
}

bool Compiler::is_toplevel_scope(Ast_Scope *scope) {
    if (scope == this->global_scope)  return true;
    if (scope == this->preload_scope) return true;

    for (auto module: this->loaded_imports) {
        if (scope == module->imported_scope) return true;
    }

    return false;
}

// Expression construction stuff, primarily used by sema and clang_import

Ast_Expression *cast_int_to_int(Compiler *compiler, Ast_Expression *expr, Ast_Type_Info *target) {
    while (expr->substitution) expr = expr->substitution;

    assert(is_int_type(expr->type_info));
    assert(is_int_type(target));

    if (target->size == expr->type_info->size) return expr;

    Ast_Cast *cast = COMPILER_NEW2(Ast_Cast);
    copy_location_info(cast, expr);
    cast->expression = expr;
    // cast->target_type_inst = nullptr;
    cast->type_info = target;
    return cast;
}

Ast_Expression *cast_float_to_float(Compiler *compiler, Ast_Expression *expr, Ast_Type_Info *target) {
    while (expr->substitution) expr = expr->substitution;

    assert(is_float_type(expr->type_info));
    assert(is_float_type(target));

    if (target->size == expr->type_info->size) return expr;

    Ast_Cast *cast = COMPILER_NEW2(Ast_Cast);
    copy_location_info(cast, expr);
    cast->expression = expr;
    // cast->target_type_info = nullptr;
    cast->type_info = target;
    return cast;
}

Ast_Expression *cast_int_to_float(Compiler *compiler, Ast_Expression *expr, Ast_Type_Info *target) {
    while (expr->substitution) expr = expr->substitution;

    assert(is_int_type(expr->type_info));
    assert(is_float_type(target));

    Ast_Cast *cast = COMPILER_NEW2(Ast_Cast);
    copy_location_info(cast, expr);
    cast->expression = expr;
    // cast->target_type_info = nullptr;
    cast->type_info = target;
    return cast;

}

Ast_Expression *cast_ptr_to_ptr(Compiler *compiler, Ast_Expression *expr, Ast_Type_Info *target) {
    while (expr->substitution) expr = expr->substitution;

    assert(is_pointer_type(expr->type_info));
    assert(is_pointer_type(target));

    Ast_Cast *cast = COMPILER_NEW2(Ast_Cast);
    copy_location_info(cast, expr);
    cast->expression = expr;
    cast->type_info = target;
    return cast;
}

Ast_Literal *make_string_literal(Compiler *compiler, String value, Ast *source_loc) {
    Ast_Literal *lit = COMPILER_NEW2(Ast_Literal);
    lit->literal_type = Ast_Literal::STRING;
    lit->string_value = value;
    lit->type_info = compiler->type_string;

    if (source_loc) copy_location_info(lit, source_loc);
    return lit;
}

Ast_Literal *make_integer_literal(Compiler *compiler, s64 value, Ast_Type_Info *type_info, Ast *source_loc) {
    Ast_Literal *lit = COMPILER_NEW2(Ast_Literal);
    lit->literal_type = Ast_Literal::INTEGER;
    lit->integer_value = value;
    lit->type_info = type_info;

    if (source_loc) copy_location_info(lit, source_loc);
    return lit;
}

Ast_Literal *make_float_literal(Compiler *compiler, double value, Ast_Type_Info *type_info, Ast *source_loc) {
    Ast_Literal *lit = COMPILER_NEW2(Ast_Literal);
    lit->literal_type = Ast_Literal::FLOAT;
    lit->float_value = value;
    lit->type_info = type_info;

    if (source_loc) copy_location_info(lit, source_loc);
    return lit;
}

Ast_Literal *make_bool_literal(Compiler *compiler, bool value, Ast *source_loc) {
    Ast_Literal *lit = COMPILER_NEW2(Ast_Literal);
    lit->literal_type = Ast_Literal::BOOL;
    lit->bool_value = value;
    lit->type_info = compiler->type_bool;

    if (source_loc) copy_location_info(lit, source_loc);
    return lit;
}

Ast_Literal *make_null_literal(Compiler *compiler, Ast_Type_Info *pointer_type, Ast *source_loc) {
    Ast_Literal *lit = COMPILER_NEW2(Ast_Literal);
    lit->literal_type = Ast_Literal::NULLPTR;
    lit->type_info = pointer_type;

    if (source_loc) copy_location_info(lit, source_loc);
    return lit;
}


Ast_Identifier *make_identifier(Compiler *compiler, Atom *name) {
    Ast_Identifier *ident = COMPILER_NEW2(Ast_Identifier);
    ident->name = name;
    return ident;
}

// @Note MUST typecheck the return value of this!!!
Ast_Array_Dereference *make_array_index(Compiler *compiler, Ast_Expression *array, Ast_Expression *index) {
    Ast_Array_Dereference *deref = COMPILER_NEW2(Ast_Array_Dereference);
    deref->array_or_pointer_expression = array;
    deref->index_expression = index;

    copy_location_info(deref, array);
    return deref;
}

// @Note MUST typecheck the return value of this!!!
Ast_Dereference *make_dereference(Compiler *compiler, Ast_Expression *aggregate_expression, Atom *field) {
    auto ident = make_identifier(compiler, field);
    copy_location_info(ident, aggregate_expression);

    Ast_Dereference *deref = COMPILER_NEW2(Ast_Dereference);
    copy_location_info(deref, aggregate_expression);
    deref->left = aggregate_expression;
    deref->field_selector = ident;
    return deref;
}

// @Note MUST typecheck the return value of this!!!
Ast_Unary_Expression *make_unary(Compiler *compiler, Token::Type op, Ast_Expression *subexpr) {
    Ast_Unary_Expression *un = COMPILER_NEW2(Ast_Unary_Expression);
    un->operator_type = op;
    un->expression = subexpr;

    copy_location_info(un, subexpr);
    return un;
}

Ast_Binary_Expression *make_binary(Compiler *compiler, Token::Type op, Ast_Expression *left, Ast_Expression *right, Ast *location) {
    Ast_Binary_Expression *bin = COMPILER_NEW2(Ast_Binary_Expression);
    bin->operator_type = op;
    bin->left = left;
    bin->right = right;
    if (location) copy_location_info(bin, location);

    return bin;
}

Ast_Type_Info *get_final_type(Ast_Type_Info *info) {
    if (!info) return nullptr;

    while (info->type == Ast_Type_Info::ALIAS) info = info->alias_of;

    return info;
}