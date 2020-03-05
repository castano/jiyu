#include "general.h"

#include "compiler.h"
#include "lexer.h"
#include "parser.h"
#include "llvm.h"
#include "sema.h"
#include "copier.h"
#include "os_support.h"
#include "clang_import.h"
#include "compiler_api.h"

#ifdef WIN32
#pragma warning(push, 0)
#endif

#include "llvm/Target/TargetMachine.h"
#include "llvm/ADT/Triple.h"

#ifdef WIN32
#pragma warning(pop)
#endif

#ifdef WIN32
#include "microsoft_craziness.h"
#endif


static String __default_module_search_path;  // @ThreadSafety
static s64    __compiler_instance_count = 0; // @ThreadSafety

// @Cleanup this gets set by the main.cpp driver, and the intention is
// that all compiler instances created will use the same target triple,
// but that is not useful, because the main.cpp driver and user metaprograms
// can just set the target triple themselves. -josh 14 January 2020
String __default_target_triple;

bool read_entire_file(String filepath, String *result) {
    char *cpath = to_c_string(filepath);
    defer { free(cpath); };

    FILE *file = fopen(cpath, "rb");
    if (!file) {
        return false;
    }
    defer { fclose(file); };

    fseek(file, 0, SEEK_END);
    auto size = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *mem = (char *)malloc(size);
    auto bytes_read = fread(mem, 1, size, file);
    if (bytes_read != (size_t)size) {
        free(mem);
        return false;
    }

    String s;
    s.data = mem;
    s.length = size;
    *result = s;
    return true;
}

static
void perform_load_from_string(Compiler *compiler, String source, Ast_Scope *target_scope) {
    Lexer *lexer = new Lexer(compiler, source, to_string(""));
    lexer->tokenize_text();

    if (compiler->errors_reported) return;

    Parser *parser = new Parser(lexer);
    parser->parse_scope(target_scope, false);

    delete lexer;
    delete parser;
}

void perform_load(Compiler *compiler, Ast *ast, String filename, Ast_Scope *target_scope) {
    String source;
    bool success = read_entire_file(filename, &source);
    if (!success) {
        compiler->report_error(ast, "Could not open file: %.*s\n", (int)filename.length, filename.data);
        return;
    }

    Lexer *lexer = new Lexer(compiler, source, filename);
    lexer->tokenize_text();

    if (compiler->errors_reported) return;

    Parser *parser = new Parser(lexer);
    parser->parse_scope(target_scope, false);

    delete lexer;
    delete parser;
}

static
u8 *get_command_line(Array<String> *strings) {
    string_length_type total_length = 0;

    for (String s : *strings) total_length += s.length;

    total_length += strings->count * 3 + 1; // enough space for quotes and a space around each argument

    string_length_type cursor = 0;
    u8 *final = reinterpret_cast<u8 *>(malloc(total_length));

    for (String s : *strings) {
        final[cursor++] = '\"';

        memcpy(final + cursor, s.data, s.length);
        cursor += s.length;

        final[cursor++] = '\"';
        final[cursor++] = ' ';
    }

    final[cursor++] = 0;
    return final;
}

static
const char *preload_text = R"C01N(

func __strings_match(a: string, b: string) -> bool {
    if (a.length != b.length) return false;
    if (a.data == null && b.data == null) return true;

    if (a.data == null) return false;
    if (b.data == null) return false;


    for 0..<a.length {
        if (a[it] != b[it]) return false;
    }

    return true;
}

)C01N";

extern "C" {
    EXPORT String compiler_system_get_default_module_search_path() {
        return copy_string(__default_module_search_path);
    }

    EXPORT void   compiler_system_set_default_module_search_path(String path) {
        __default_module_search_path = copy_string(path);
    }

    EXPORT Compiler *create_compiler_instance(Build_Options *options) {
        auto compiler = new Compiler();

        compiler->instance_number = __compiler_instance_count++;

        // these are copies to prevent the user program from modifying the strings after-the-fact.
        compiler->build_options.executable_name = copy_string(options->executable_name);
        if (options->target_triple != String()) {
            compiler->build_options.target_triple   = copy_string(options->target_triple);
        } else {
            compiler->build_options.target_triple   = __default_target_triple;
        }
        compiler->build_options.only_want_obj_file  = options->only_want_obj_file;
        compiler->build_options.verbose_diagnostics = options->verbose_diagnostics;
        compiler->build_options.emit_llvm_ir        = options->emit_llvm_ir;

        compiler->llvm_gen = new LLVM_Generator(compiler);
        compiler->llvm_gen->preinit();

        compiler->init();

        compiler->jitter = new LLVM_Jitter(compiler->llvm_gen);

        compiler->copier = new Copier(compiler);

        compiler->sema = new Sema(compiler);

        if (__default_module_search_path != String()) {
            compiler->module_search_paths.add(__default_module_search_path);
            compiler->library_search_paths.add(__default_module_search_path);
        }

        perform_load_from_string(compiler, to_string((char *)preload_text), compiler->preload_scope);

        // Insert some built-in definitions so code can reason about the types of array and string fields.
        {
            Ast_Identifier *ident = make_identifier(compiler, compiler->make_atom(to_string("__builtin_string_length_type")));
            Ast_Type_Alias *alias = make_type_alias(compiler, ident, compiler->type_string_length);

            compiler->preload_scope->statements.add(alias);
            compiler->preload_scope->declarations.add(alias);

            ident = make_identifier(compiler, compiler->make_atom(to_string("__builtin_array_count_type")));
            alias = make_type_alias(compiler, ident, compiler->type_array_count);

            compiler->preload_scope->statements.add(alias);
            compiler->preload_scope->declarations.add(alias);
        }

        // Insert intrinsic functions
        {
            Ast_Identifier *ident = make_identifier(compiler, compiler->atom_builtin_debugtrap);
            auto debug_trap = make_function(compiler, ident);
            debug_trap->is_intrinsic = true;
            compiler->preload_scope->statements.add(debug_trap);
            compiler->preload_scope->declarations.add(debug_trap);
        }

        os_init(compiler);
        return compiler;
    }

    EXPORT void destroy_compiler_instance(Compiler *compiler) {
        delete compiler->sema;
        delete compiler->copier;
        delete compiler->llvm_gen;
        delete compiler->jitter;
        delete compiler->atom_table;
        delete compiler;
    }

    EXPORT Build_Options *compiler_get_build_options(Compiler *compiler) {
        return &compiler->build_options;
    }

    EXPORT bool compiler_run_default_link_command(Compiler *compiler) {
        if (compiler->build_options.only_want_obj_file) return true;
        if (compiler->build_options.executable_name == to_string("")) return false;

#if WIN32
        Find_Result win32_sdk;
        find_visual_studio_and_windows_sdk(&win32_sdk);

        if (win32_sdk.vs_exe_path) {
            const int LINE_SIZE = 4096;
            char exe_path[LINE_SIZE]; // @Cleanup hardcoded value
            char libpath[LINE_SIZE];

            Array<String> args;

            snprintf(exe_path, LINE_SIZE, "%S\\link.exe", win32_sdk.vs_exe_path);
            args.add(to_string(exe_path));

            if (win32_sdk.vs_library_path) {

                snprintf(libpath, LINE_SIZE, "/libpath:%S", win32_sdk.vs_library_path);
                args.add(copy_string(to_string(libpath)));
            }

            if (win32_sdk.windows_sdk_um_library_path) {
                snprintf(libpath, LINE_SIZE, "/libpath:%S", win32_sdk.windows_sdk_um_library_path);
                args.add(copy_string(to_string(libpath)));
            }

            if (win32_sdk.windows_sdk_ucrt_library_path) {
                snprintf(libpath, LINE_SIZE, "/libpath:%S", win32_sdk.windows_sdk_ucrt_library_path);
                args.add(copy_string(to_string(libpath)));
            }

            for (auto path: compiler->library_search_paths) {
                snprintf(libpath, LINE_SIZE, "/libpath:%.*s", PRINT_ARG(path));
                args.add(copy_string(to_string(libpath)));
            }

            args.add(to_string("/NODEFAULTLIB:libcmt"));

            if (compiler->libraries.count == 0) {
                // add default C library if no libraries are specified. This way we dont get:
                // LINK : error LNK2001: unresolved external symbol mainCRTStartup

                args.add(to_string("msvcrt.lib"));
            }

            for (auto lib: compiler->libraries) {
                String s = lib->libname;
                args.add(mprintf("%.*s.lib", PRINT_ARG(s)));
            }

            args.add(to_string("/nologo"));
            args.add(to_string("/DEBUG"));

            String exec_name = compiler->build_options.executable_name;
            String obj_name = mprintf("%.*s.o", PRINT_ARG(exec_name));
            convert_to_back_slashes(obj_name);
            args.add(obj_name);

            for (auto obj: compiler->user_supplied_objs) {
                auto temp = copy_string(obj); // @Leak
                convert_to_back_slashes(temp);
                args.add(obj);
            }

            String output_name = exec_name;
            char executable_name[LINE_SIZE];
            snprintf(executable_name, LINE_SIZE, "/OUT:%.*s.exe", PRINT_ARG(output_name));
            convert_to_back_slashes(executable_name + 1);

            args.add(to_string(executable_name));
            // args.add(to_string("/Fe:"));
            // args.add(compiler->executable_name);


            auto cmd_line = get_command_line(&args);
            if (compiler->build_options.verbose_diagnostics) printf("Linker line: %s\n", cmd_line);

            // system((char *)cmd_line);
            STARTUPINFOA startup;
            memset(&startup, 0, sizeof(STARTUPINFOA));
            startup.cb = sizeof(STARTUPINFOA);
            startup.dwFlags    = STARTF_USESTDHANDLES;
            startup.hStdInput  = GetStdHandle(STD_INPUT_HANDLE);
            startup.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
            startup.hStdError  = GetStdHandle(STD_ERROR_HANDLE);

            PROCESS_INFORMATION process_info;
            CreateProcessA(nullptr, (char *) cmd_line, nullptr, nullptr, TRUE, 0, nullptr, nullptr, &startup, &process_info);
            WaitForSingleObject(process_info.hProcess, INFINITE);

            free(cmd_line);
            free(obj_name.data);
        }
#else
        // @Incomplete should use the execpve family
        Array<String> args;
        args.add(to_string("ld"));

        String exec_name = compiler->build_options.executable_name;
        String obj_name = mprintf("%.*s.o", exec_name.length, exec_name.data);

        args.add(obj_name);

        for (auto obj: compiler->user_supplied_objs) {
            args.add(obj);
        }

        auto triple = compiler->llvm_gen->TargetMachine->getTargetTriple();
        if (triple.isOSLinux()) {
            // CRT files
            // these seem to be GCC specific
//             auto crtbegin = compiler->find_file_in_library_search_paths(to_string("crtbegin.o")); // @Leak
//             auto crtend   = compiler->find_file_in_library_search_paths(to_string("crtend.o")); // @Leak
            auto crti     = compiler->find_file_in_library_search_paths(to_string("crti.o")); // @Leak
            auto crtn     = compiler->find_file_in_library_search_paths(to_string("crtn.o")); // @Leak
            auto crt1     = compiler->find_file_in_library_search_paths(to_string("crt1.o")); // @Leak

            if (compiler->build_options.verbose_diagnostics) {
//                 if (!crtbegin.item1) printf("Could not find crtbegin.\n");
//                 if (!crtend.item1)   printf("Could not find crtend.\n");
                if (!crti.item1)     printf("Could not find crti.o.\n");
                if (!crtn.item1)     printf("Could not find crtn.o.\n");
                if (!crt1.item1)     printf("Could not find crt1.o.\n");
            }

//             if (crtbegin.item1) args.add(crtbegin.item2);
//             if (crtend.item1)   args.add(crtend.item2);
            if (crti.item1) args.add(crti.item2);
            if (crtn.item1) args.add(crtn.item2);
            if (crt1.item1) args.add(crt1.item2);

            args.add(to_string("--eh-frame-hdr"));

            // dynamic linker
            args.add(to_string("-dynamic-linker"));
            // @TODO clang also checks if the user supplied arguments to use hardfloats. Maybe we can query that through TargetMachine..
            auto arch = triple.getArch();

            // @TODO aarch64 and others.
            // @TODO MUSL
            using namespace llvm;
            if (triple.isARM() || triple.isThumb()) {
                bool hardfloats = (triple.getEnvironment() == Triple::GNUEABIHF);

                String linker_name = to_string("/lib/ld-linux.so.3");
                if (hardfloats) linker_name = to_string("/lib/ld-linux-armhf.so.3");

                args.add(linker_name);
            } else if (arch == Triple::x86_64) {
                // @TODO this probably isnt correct if you're compiling 32-bit programs on 64-bit OS.
                String linker_string = to_string("/lib64/ld-linux-x86-64.so.2");
                args.add(linker_string);
            } else {
                if (compiler->build_options.verbose_diagnostics) {
                    printf("Could not determine the dynamic linker needed for this platform.\n");
                }
            }
        }

        args.add(to_string("-o"));
        args.add(compiler->build_options.executable_name);

        for (auto path: compiler->library_search_paths) {
            args.add(to_string("-L"));
            args.add(path);
        }

        for (auto lib: compiler->libraries) {
            if (lib->is_framework) {
                args.add(to_string("-framework"));
                args.add(lib->libname);
            } else {
                String s = lib->libname;
                args.add(mprintf("-l%.*s", s.length, s.data)); // @Leak
            }
        }

        auto cmd_line = get_command_line(&args);
        if (compiler->build_options.verbose_diagnostics) printf("Linker line: %s\n", cmd_line);
        system((char *)cmd_line);

        free(cmd_line);
        free(obj_name.data);

        if (triple.isOSDarwin()) {
            args.reset();
            args.add(to_string("dsymutil"));
            args.add(exec_name);

            auto cmd_line = get_command_line(&args);
            if (compiler->build_options.verbose_diagnostics) printf("dsymutil line: %s\n", cmd_line);
            system((char *)cmd_line);

            free(cmd_line);
        }
#endif

        // @TODO make sure we successfully launch the link command and that it returns a success code
        return true;
    }

    EXPORT bool compiler_load_file(Compiler *compiler, String filename) {
        perform_load(compiler, nullptr, filename, compiler->global_scope);

        return compiler->errors_reported == 0;
    }

    EXPORT bool compiler_load_string(Compiler *compiler, String source) {
        perform_load_from_string(compiler, source, compiler->global_scope);

        return compiler->errors_reported == 0;
    }

    EXPORT bool compiler_typecheck_program(Compiler *compiler) {
        compiler->resolve_directives();
        if (compiler->errors_reported) return false;

        assert(compiler->directive_queue.count == 0);

        compiler->sema->typecheck_scope(compiler->preload_scope);
        compiler->sema->typecheck_scope(compiler->global_scope);

        for (auto import: compiler->loaded_imports) {
            // assert(import->imported_scope->type_info);
            compiler->sema->typecheck_scope(import->imported_scope);
        }

        // set metaprogram status if main is marked @metaprogram
        if (!compiler->errors_reported && !compiler->build_options.only_want_obj_file) {
            auto expr = compiler->sema->find_declaration_for_atom_in_scope(compiler->global_scope, compiler->atom_main);

            if (!expr) {
                compiler->report_error((Token *)nullptr, "No function 'main' found in global scope. Aborting.\n");
                return false;
            }

            if (expr->type != AST_FUNCTION) {
                compiler->report_error(expr, "Expected a function named 'main' in global scope, but got something else.\n");
                return false;
            }

            Ast_Function *function = static_cast<Ast_Function *>(expr);
            if (function->is_marked_metaprogram) compiler->is_metaprogram = true;
            function->is_exported = true; // main() must be exported by default
        }

        return compiler->errors_reported == 0;
    }

    EXPORT bool compiler_generate_llvm_module(Compiler *compiler) {
        compiler->llvm_gen->init();

        for (auto decl: compiler->global_decl_emission_queue) {
            assert(!decl->is_let);
            assert(compiler->is_toplevel_scope(decl->identifier->enclosing_scope));

            compiler->llvm_gen->emit_global_variable(decl);
        }

        for (auto &function : compiler->function_emission_queue) {
            compiler->llvm_gen->emit_function(function);
        }

        return compiler->errors_reported == 0;
    }

    EXPORT bool compiler_emit_object_file(Compiler *compiler) {
        compiler->llvm_gen->finalize();
        return compiler->errors_reported == 0;
    }

    EXPORT bool compiler_jit_program(Compiler *compiler) {
        compiler->jitter->init();
        return compiler->errors_reported == 0;
    }

    EXPORT void *compiler_jit_lookup_symbol(Compiler *compiler, String symbol_name) {
        return compiler->jitter->lookup_symbol(symbol_name);
    }

    EXPORT void compiler_add_library_search_path(Compiler *compiler, String path) {
        compiler->library_search_paths.add(copy_string(path)); // @Leak @Deduplicate
    }

    EXPORT void compiler_add_module_search_path(Compiler *compiler, String path) {
        compiler->module_search_paths.add(copy_string(path)); // @Leak @Deduplicate
    }

    EXPORT void compiler_add_compiled_object_for_linking(Compiler *compiler, String path) {
        compiler->user_supplied_objs.add(copy_string(path));
    }

    EXPORT String_Array compiler_get_loaded_files(Compiler *compiler, String filename) {
        Array<String> array;
        array.resize(compiler->loaded_imports.count);
       
        for (int i = 0; i < array.count; i++) {
            array.data[i] = compiler->loaded_imports[i]->target_filename;
        }

        String_Array result = {array.data, array.count};
        return result;
        //return MakeTuple(array.data, array.count);
    }

}
