
#ifndef OS_SUPPORT_H
#define OS_SUPPORT_H

#include "general.h"

String get_executable_path();

bool file_exists(String path);

bool is_debugger_present();

#endif // OS_SUPPORT_H
