cmake_minimum_required(VERSION 2.6)
project(jiyu)
find_package (Threads)

set(LLVM_DIR "${PROJECT_SOURCE_DIR}/llvm/lib/cmake/llvm/")
find_package(LLVM REQUIRED CONFIG)

# sigh
if (WIN32)
    find_library(LIBCLANG libclang HINTS ${PROJECT_SOURCE_DIR}/llvm/lib)
else ()
    find_library(LIBCLANG clang HINTS ${PROJECT_SOURCE_DIR}/llvm/lib)
endif ()

if(NOT CMAKE_SIZEOF_VOID_P EQUAL 8)
    add_definitions(-DENV32)
else()
    add_definitions(-DENV64)
endif()

#include_directories()

set (JIYU_SRCS
    src/main.cpp
    src/parser.cpp
    src/lexer.cpp
    src/compiler.cpp
    src/llvm.cpp
    src/sema.cpp
    src/copier.cpp
    src/os_support.cpp
    src/clang_import.cpp
)

if (MSVC)
	LIST(APPEND JIYU_SRCS
		src/ast.h
		src/clang_import.h
		src/compiler.h
		src/copier.h
		src/general.h
		src/lexer.h
		src/llvm.h
		src/meow_hash.h
		src/meow_intrinsics.h
		src/microsoft_craziness.h
		src/os_support.h
		src/parser.h
		src/sema.h
	)
endif()


if (UNIX AND NOT APPLE)
    set(LINUX TRUE)
endif ()

# compiler options
if (WIN32)
# @Cleanup is there an easier way to get the CRT that LLVM was configured to use? 
	set(CMAKE_CXX_FLAGS_RELEASE        "${CMAKE_CXX_FLAGS_RELEASE}        /MT")
	set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} /MT")	
	set(CMAKE_CXX_FLAGS_DEBUG          "${CMAKE_CXX_FLAGS_DEBUG}          /MT")
else()
	add_definitions(-Wno-writable-strings --std=c++11 -g -fno-rtti -Wall -Wextra -pedantic -Wno-unused-parameter)
endif()

if (APPLE)
    add_definitions(-DMACOSX -DUNIX)
endif()

if (LINUX)
    add_definitions(-DLINUX  -DUNIX)
endif ()


include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

llvm_map_components_to_libnames(LLVM_LIBS ${LLVM_TARGETS_TO_BUILD} asmparser asmprinter passes orcjit)
add_executable(jiyu ${JIYU_SRCS})
target_link_libraries(jiyu ${CMAKE_THREAD_LIBS_INIT} ${LLVM_LIBS} ${LIBCLANG})
