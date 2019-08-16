
#include "os_support.h"
#include "general.h"

#ifdef WIN32

#include <windows.h>
#include <shlwapi.h>

#pragma comment(lib, "shlwapi.lib") // For PathFileExistsA

// :UseWVersions:
// We should be using the W versions of Win32 API functions
// since we want to fully support Unicode strings, but using
// the A-versions is simpler in the short term.


String get_executable_path() {
    const DWORD BUFFER_SIZE = 512;
    char buf[BUFFER_SIZE];

    auto module = GetModuleHandleA(nullptr);
    GetModuleFileNameA(module, buf, BUFFER_SIZE); // @Cleanup :UseWVersions:

    convert_to_forward_slashes(buf);
    return copy_string(to_string(buf));
}

bool file_exists(String path) {
	char *c_str = to_c_string(path);
	convert_to_back_slashes(c_str);

	BOOL result = PathFileExistsA(c_str);
	free(c_str);

	return result == TRUE;
}
#endif

#ifdef MACOSX
#include <mach-o/dyld.h>

String get_executable_path() {
    const u32 BUFFER_SIZE = 512;
    char buf[BUFFER_SIZE];

    u32 bufsize = BUFFER_SIZE;
    auto result = _NSGetExecutablePath(buf, &bufsize);
    if (result != 0) return to_string("");

    return copy_string(to_string(buf));
}
#endif

// @Incomplete get_executable_path for Linux

#ifdef UNIX
#include <unistd.h>
#include <sys/stat.h>

bool file_exists(String path) {
	char *c_str = to_c_string(path);
	bool result = access(c_str, F_OK) != -1;
	free(c_str);
	return result;
}
#endif // UNIX
