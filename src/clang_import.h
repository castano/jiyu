
#include "general.h"
#include "ast.h"

bool perform_clang_import(Compiler *compiler, char *c_filepath, Ast_Scope *target_scope);
bool perform_clang_import_string(Compiler *, String string_to_compile, Ast_Scope *target_scope);