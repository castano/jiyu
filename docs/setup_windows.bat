

set JIYU_DIR=%cd%

set LLVM_VERSION=release_80
set LLVM_URL=https://github.com/llvm-mirror/llvm/archive/%LLVM_VERSION%.zip
set CLANG_URL=https://github.com/llvm-mirror/clang/archive/%LLVM_VERSION%.zip

if "%LLVM_TARGETS%" == "" (
    set LLVM_TARGETS=X86;ARM;AArch64
)

if not exist llvm-%LLVM_VERSION%.src.zip (
    wget %LLVM_URL% || curl -L --output llvm-%LLVM_VERSION%.src.zip %LLVM_URL%
)

if not exist clang-%LLVM_VERSION%.src.zip (
    wget %CLANG_URL% || curl -L --output clang-%LLVM_VERSION%.src.zip %CLANG_URL%
)

if not exist llvm-%LLVM_VERSION% (
    unzip  llvm-%LLVM_VERSION%.src.zip
)

if not exist llvm-%LLVM_VERSION%/tools/clang (
    unzip clang-%LLVM_VERSION%.src.zip
    mv clang-%LLVM_VERSION% llvm-%LLVM_VERSION%/tools/clang
)

cd llvm-%LLVM_VERSION%
mkdir build
cd build

cmake .. -G "Visual Studio 15 2017 Win64" -Thost=x64 -DCMAKE_BUILD_TYPE=RelWithDebInfo -DLLVM_TARGETS_TO_BUILD=%LLVM_TARGETS% -DLLVM_ENABLE_DUMP=ON -DCMAKE_INSTALL_PREFIX=%JIYU_DIR%\llvm -DLLVM_USE_CRT_RELEASE=MT -DLLVM_USE_CRT_RELWITHDEBINFO=MT -DLLVM_USE_CRT_DEBUG=MTd
cmake --build . --config RelWithDebInfo --target install

cd %JIYU_DIR%

