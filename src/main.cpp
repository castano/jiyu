
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

extern String __default_module_search_path;
extern String __default_target_triple;

static
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
    bool verbose = false;
    String target_triple;
    char *import_c_file = nullptr;
    bool emit_llvm_ir = false;

    int metaprogram_arg_start = -1;

    for (int i = 1; i < argc; ++i) {
        if (to_string("-meta") == to_string(argv[i])) {
            is_metaprogram = true;
        } else if (to_string("-v") == to_string(argv[i])) {
            verbose = true;
        } else if (to_string("-c") == to_string(argv[i])) {
            only_want_obj_file = true;
        } else if (to_string("-emit-llvm") == to_string(argv[i])) {
            emit_llvm_ir = true;
        } else if (to_string("-o") == to_string(argv[i])) {
            if (i+1 < argc) {
                output_name = to_string(argv[i+1]);
                i++;
            } else {
                printf("error: No output name specified, following -o switch.\n");
                return -1;
            }
        } else if (to_string("-clang_import") == to_string(argv[i])) {
            if (i+1 < argc) {
                import_c_file = argv[i+1];
                i++;
            } else {
                printf("error: No C file name specified, following -clang_import switch.\n");
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

    __default_target_triple     = target_triple;
    options.target_triple       = target_triple;
    options.only_want_obj_file  = only_want_obj_file;
    options.verbose_diagnostics = verbose;
    options.emit_llvm_ir        = emit_llvm_ir;

    auto compiler = create_compiler_instance(&options);
    compiler->is_metaprogram = is_metaprogram;

    Array<char *> metaprogram_arguments;
    if (metaprogram_arg_start >= 0) {
        for (int i = metaprogram_arg_start; i < argc; ++i) {
            metaprogram_arguments.add(argv[i]);
        }
    }

    if (filename == String()) {
        compiler->report_error((Token *)nullptr, "No input files specified.\n");
        return -1;
    }

    if (import_c_file) {
        if (!perform_clang_import(compiler, import_c_file, compiler->global_scope)) return -1;
    }

    if (!compiler_load_file(compiler, filename)) return -1;

    if (!compiler_typecheck_program(compiler)) return -1;

    if (!compiler_generate_llvm_module(compiler)) return -1;

    if (compiler->is_metaprogram) {
        if (!compiler_jit_program(compiler)) return -1;

        String symbol = to_string("main");
        auto *Main = (void (*)(s32 argc, char **argv)) compiler_jit_lookup_symbol(compiler, symbol);
        if (!Main) {
            compiler->report_error((Ast *)nullptr, "Could lookup symbol for program entry point 'main'.\n");
            return -1;
        }
        Main(metaprogram_arguments.count, metaprogram_arguments.data);
        return 0;
    }

    if (!compiler_emit_object_file(compiler)) return -1;;

    if (!compiler_run_default_link_command(compiler)) return -1;

    return 0;
}
