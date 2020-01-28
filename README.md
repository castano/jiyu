# jiyu
A compiler for fun.

## Setup
Clone this repository:
```
git clone https://github.com/machinamentum/jiyu.git
cd jiyu
```
### Fast Setup (macOS and Ubuntu)
On macOS and several versions of Ubuntu, prebuilt distributions of LLVM can just be downloaded from llvm.org:
```
docs/setup_unix_fast.sh
mkdir build
cd build
cmake ..
cmake --build .
cd ..
```
### Manual Setup
Build LLVM and jiyu:
##### Windows
```
docs\setup_windows.bat
mkdir build
cd build
cmake .. -G "Visual Studio 15 2017 Win64" -Thost=x64
cmake --build .
cd ..
```
##### Unix-like Systems
```
docs/setup_unix.sh
mkdir build
cd build
cmake ..
cmake --build .
cd ..
```
`docs\setup_windows.bat` and `docs/setup_unix.sh` fetch, build, and install LLVM into \<jiyu\>/llvm. This may take awhile, go enjoy a walk on the beach or something!

## Examples
The `tests` directory contain several isolated code examples for verifying compiler functionality. For a more in-depth example, see [jiyu_game](https://github.com/machinamentum/jiyu_game).
