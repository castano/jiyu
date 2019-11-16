
#include "general.h"

#include "compiler.h"
#include "lexer.h"
#include "parser.h"
#include "llvm.h"
#include "sema.h"
#include "copier.h"
#include "os_support.h"

#ifdef WIN32
#include "microsoft_craziness.h"

#define EXPORT __declspec(dllexport)
#else
#define EXPORT 
#endif

#include <stdio.h>

String TextSpan::get_text() {
    String s;
    s.data = string.data + span.start;
    s.length = span.length;
    return s;
}

String mprintf(char *c_fmt, ...) {
    String_Builder builder;
    
    va_list vl;
    va_start(vl, c_fmt);
    builder.print_valist(c_fmt, vl);
    va_end(vl);
    
    return builder.to_string();
}

bool read_entire_file(String filepath, String *result) {
    char *cpath = to_c_string(filepath);
    
    FILE *file = fopen(cpath, "rb");
    if (!file) {
        free(cpath);
        return false;
    }
    
    fseek(file, 0, SEEK_END);
    auto size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    char *mem = (char *)malloc(size);
    auto bytes_read = fread(mem, 1, size, file);
    if (bytes_read != (size_t)size) {
        fclose(file);
        free(mem);
        free(cpath);
        return false;
    }
    
    String s;
    s.data = mem;
    s.length = size;
    *result = s;
    free(cpath);
    return true;
}

void perform_load_from_string(Compiler *compiler, String source, Ast_Scope *target_scope) {
    Lexer *lexer = new Lexer(compiler, source, to_string(""));
    lexer->tokenize_text();
    
    if (compiler->errors_reported) return;
    
    Parser *parser = new Parser(lexer);
    parser->parse_scope(target_scope, false);
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
}

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

const char *preload_text = R"C01N(

func __strings_match(a: string, b: string) -> bool {
    if (a.length != b.length) return false;
    if (a.data == null && b.data != null) return false;
    if (b.data == null && a.data != null) return false;
    if (a.data == null && b.data == null) return true;
    
    for 0..a.length-1 {
        if (a[it] != b[it]) return false;
    }
    
    return true;
}

)C01N";

String __default_module_search_path;
s64    __compiler_instance_count = 0; // @ThreadSafety


extern "C" {
    EXPORT Compiler *create_compiler_instance(Build_Options *options) {
        auto compiler = new Compiler();

        compiler->instance_number = __compiler_instance_count++;

        // these are copies to prevent the user program from modifying the strings after-the-fact.
        compiler->build_options.executable_name = copy_string(options->executable_name);
        compiler->build_options.target_triple   = copy_string(options->target_triple);
        compiler->build_options.only_want_obj_file = options->only_want_obj_file;

        compiler->llvm_gen = new LLVM_Generator(compiler);
        compiler->llvm_gen->preinit();

        compiler->init();
        
        compiler->copier = new Copier(compiler);
        
        compiler->sema = new Sema(compiler);
        
        compiler->llvm_gen->init();

        compiler->module_search_paths.add(__default_module_search_path);
        
        perform_load_from_string(compiler, to_string((char *)preload_text), compiler->preload_scope);
        
        return compiler;
    }
    
    EXPORT bool compiler_run_default_link_command(Compiler *compiler) {
        if (compiler->build_options.only_want_obj_file) return true;
        if (compiler->build_options.executable_name == to_string("")) return false;

#if WIN32
        auto win32_sdk = find_visual_studio_and_windows_sdk();
        
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
            
            args.add(to_string("/NODEFAULTLIB:libcmt"));
            
            if (compiler->libraries.count == 0) {
                // add default C library if no libraries are specified. This way we dont get:
                // LINK : error LNK2001: unresolved external symbol mainCRTStartup
                
                args.add(to_string("msvcrt.lib"));
            }
            
            for (auto lib: compiler->libraries) {
                String s = lib->libname;
                args.add(mprintf("%.*s.lib", s.length, s.data));
            }
            
            args.add(to_string("/nologo"));
            args.add(to_string("/DEBUG"));
            
            String exec_name = compiler->build_options.executable_name;
            String obj_name = mprintf("%.*s.o", exec_name.length, exec_name.data);
            args.add(obj_name);
            
            String output_name = exec_name;
            char executable_name[LINE_SIZE];
            snprintf(executable_name, LINE_SIZE, "/OUT:%.*s.exe", output_name.length, output_name.data);
            convert_to_back_slashes(executable_name + 1);
            
            args.add(to_string(executable_name));
            // args.add(to_string("/Fe:"));
            // args.add(compiler->executable_name);
            
            
            auto cmd_line = get_command_line(&args);
            printf("Linker line: %s\n", cmd_line);
            
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
        
        args.add(to_string("-o"));
        args.add(compiler->build_options.executable_name);
        
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
        printf("Linker line: %s\n", cmd_line);
        system((char *)cmd_line);

        free(cmd_line);
        free(obj_name.data);
#endif
        
        // @TODO make sure we successfully launch the link command and that it returns a success code
        return true;
    }
    
    // @FixMe on macOS/Linux string should normally be passed by-value but we're currently passing string by pointer by default.
    EXPORT bool compiler_load_file(Compiler *compiler, String *filename) {
        perform_load(compiler, nullptr, *filename, compiler->global_scope);
        
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
        }
        
        return compiler->errors_reported == 0;
    }
    
    EXPORT bool compiler_generate_llvm_module(Compiler *compiler) {
        // @Incomplete global variables
        
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

    EXPORT bool compiler_run_metaprogram(Compiler *compiler, s32 argc, char **argv) {
        compiler->metaprogram_argc = argc;
        compiler->metaprogram_argv = argv;
        LLVM_Jitter *jitter = new LLVM_Jitter(compiler->llvm_gen);
        jitter->init();
        return true;
    }

    // @FixMe on macOS/Linux string should normally be passed by-value but we're currently passing string by pointer by default.
    EXPORT void compiler_add_library_search_path(Compiler *compiler, String *path) {
        compiler->library_search_paths.add(copy_string(*path)); // @TODO we should check for duplication here, maybe.
    }
}

String get_jiyu_work_directory(String exe_dir_path) {
    while (exe_dir_path.length) {
        String name = basename(exe_dir_path);

        if (name == to_string("jiyu")) {
            return exe_dir_path;
        }

        exe_dir_path = basepath(exe_dir_path);
    }

    return exe_dir_path;
}

int main(int argc, char **argv) {
    String filename;
    String output_name;
    bool is_metaprogram = false;
    bool only_want_obj_file = false;
    String target_triple;

    int metaprogram_arg_start = -1;
    
    for (int i = 1; i < argc; ++i) {
        if (to_string("-meta") == to_string(argv[i])) {
            is_metaprogram = true;
        } else if (to_string("-c") == to_string(argv[i])) {
            only_want_obj_file = true;
        } else if (to_string("-o") == to_string(argv[i])) {
            if (i+1 < argc) {
                output_name = to_string(argv[i+1]);
                i++;
            } else {
                printf("error: No output name specified, following -o switch.\n");
                return -1;
            }
        } else if (to_string("-target") == to_string(argv[i])) {
            if (i+1 < argc) {
                target_triple = to_string(argv[i+1]);
                i++;
            } else {
                printf("error: No target triple specified, following -target switch.\n");
                return -1;
            }
        } else if (to_string("--") == to_string(argv[i])) {
            metaprogram_arg_start = i+1;
            break;
        } else {
            filename = to_string(argv[i]);
        }
    }

    {
        String path = get_executable_path();
        String working_dir = get_jiyu_work_directory(basepath(path));

        __default_module_search_path = mprintf("%.*smodules", working_dir.length, working_dir.data);
        // printf("Modules path: %.*s\n", __default_module_search_path.length, __default_module_search_path.data);

        free(path.data);
    }

    Build_Options options;
    options.executable_name = output_name;
    if (options.executable_name == to_string("")) {
        options.executable_name = to_string("output");
    }

    options.target_triple = target_triple;
    options.only_want_obj_file = only_want_obj_file;
    
    auto compiler = create_compiler_instance(&options);
    compiler->is_metaprogram = is_metaprogram;

    Array<char *> metaprogram_arguments;
    if (metaprogram_arg_start >= 0) {
        for (int i = metaprogram_arg_start; i < argc; ++i) {
            metaprogram_arguments.add(argv[i]);
        }
    }
    
    if (filename == to_string("")) {
        compiler->report_error((Token *)nullptr, "No input files specified.\n");
        return -1;
    }
    
    if (!compiler_load_file(compiler, &filename)) return -1;
    
    if (!compiler_typecheck_program(compiler)) return -1;
    
    if (!compiler_generate_llvm_module(compiler)) return -1;
    
    if (compiler->is_metaprogram) {
        compiler_run_metaprogram(compiler, static_cast<s32>(metaprogram_arguments.count),  metaprogram_arguments.data);
        return 0;
    }
    
    if (!compiler_emit_object_file(compiler)) return -1;;
    
    if (!compiler_run_default_link_command(compiler)) return -1;
    
    return 0;
}
