
#include "os_support.h"
#include "general.h"
#include "compiler.h"

#ifdef WIN32

#include <windows.h>
#include <shlwapi.h>

#pragma comment(lib, "shlwapi.lib") // For PathFileExistsA

// :UseWVersions:
// We should be using the W versions of Win32 API functions
// since we want to fully support Unicode strings, but using
// the A-versions is simpler in the short term.

String get_executable_path() {
    const DWORD BUFFER_SIZE = 512;  // @@ Fixed buffers.
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

void os_init(Compiler *compiler) {
}

bool is_debugger_present() {
    return IsDebuggerPresent() == TRUE;
}

#endif

#ifdef MACOSX
#include <mach-o/dyld.h>
#include <stdlib.h>
#include <sys/sysctl.h>
#include <unistd.h> // getpid

String get_executable_path() {
    u32 bufsize = 0;
    auto result = _NSGetExecutablePath(nullptr, &bufsize);

    char * path = (char *)malloc(bufsize);
    defer { free(path); };

    result = _NSGetExecutablePath(path, &bufsize);
    if (result != 0) return String();

    char * real_path = realpath(path, nullptr);
    return to_string(real_path);
}

void os_init(Compiler *compiler) {
}

bool is_debugger_present() {
    int mib[4];
    struct kinfo_proc info;
    size_t size;
    mib[0] = CTL_KERN;
    mib[1] = KERN_PROC;
    mib[2] = KERN_PROC_PID;
    mib[3] = getpid();
    size = sizeof(info);
    info.kp_proc.p_flag = 0;
    sysctl(mib,4,&info,&size,NULL,0);
    return ((info.kp_proc.p_flag & P_TRACED) == P_TRACED);
}

#endif // MACOSX

#ifdef LINUX
#include <unistd.h>
#include <glob.h>

String get_executable_path() {
    const u32 BUFFER_SIZE = 512;    // @@ Fixed buffers.
    char buf[BUFFER_SIZE];

    u32 bufsize = BUFFER_SIZE;
    auto result = readlink("/proc/self/exe", buf, bufsize);
    if (result < 0) return String();

    char * real_path = realpath(buf, nullptr);
    return to_string(real_path);
}

static
void parse_ld_config_file_for_paths(Compiler *compiler, String filepath) {
    bool read_entire_file(String filepath, String *result);

    String result;
    bool success = read_entire_file(filepath, &result);
    if (!success) return;

    defer { free(result.data); };

    Array<String> lines;
    split(result, '\n', &lines);

    for (auto line: lines) {
        if (line.data[0] == '#') continue; // skip comments;

        Array<String> substrings;
        split(line, ' ', &substrings);

        String first = substrings[0];
        if (first == to_string("include")) {
            glob_t glob_result;
            char *temp_c_string = to_c_string(substrings[1]);
            int result = glob(temp_c_string, 0, nullptr, &glob_result);
            free(temp_c_string);

            if (result != 0) {
                printf("glob() failed with result: %d\n", result);
                continue;
            }

            for (size_t i = 0; i < glob_result.gl_pathc; ++i) {
                parse_ld_config_file_for_paths(compiler, to_string(glob_result.gl_pathv[i]));
            }

            globfree(&glob_result);
        } else {
            compiler->library_search_paths.add(compiler->copy_string(first));
        }
    }
}

void os_init(Compiler *compiler) {
    parse_ld_config_file_for_paths(compiler, to_string("/etc/ld.so.conf"));
}

#endif // LINUX


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
