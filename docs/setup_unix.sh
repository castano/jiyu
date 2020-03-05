
set -ex

JIYU_DIR=$PWD

LLVM_VERSION=9.0.0
LLVM_URL=https://github.com/llvm/llvm-project/archive/llvmorg-$LLVM_VERSION.tar.gz

if [ -z $LLVM_TARGETS ]
then
    LLVM_TARGETS="X86;ARM;AArch64"
fi

if [ -z $LLVM_BUILD_TYPE ]
then
    LLVM_BUILD_TYPE=Release
fi


if [[ ! -f llvmorg-$LLVM_VERSION.tar.gz ]]
then
    wget $LLVM_URL -O llvmorg-$LLVM_VERSION.tar.gz || curl -L --output llvmorg-$LLVM_VERSION.tar.gz $LLVM_URL
fi

if [[ ! -d llvm-project-llvmorg-$LLVM_VERSION ]]
then
    tar -vxzf llvmorg-$LLVM_VERSION.tar.gz
fi

cd llvm-project-llvmorg-$LLVM_VERSION
mkdir -p build
cd build

# Turn off as many unneeded build steps in clang+llvm as possible to speedup the build and require less resources so that this works on SBC's with a small amount of memory.
cmake ../llvm -DCMAKE_BUILD_TYPE=$LLVM_BUILD_TYPE -DLLVM_TARGETS_TO_BUILD=$LLVM_TARGETS -DLLVM_ENABLE_DUMP=ON -DCMAKE_INSTALL_PREFIX=$JIYU_DIR/llvm \
              -DLLVM_BUILD_TOOLS=OFF -DLLVM_INCLUDE_BENCHMARKS=OFF -DLLVM_INCLUDE_EXAMPLES=OFF -DLLVM_INCLUDE_TESTS=OFF -DLLVM_ENABLE_WARNINGS=OFF \
              -DCMAKE_C_FLAGS="-fPIC" -DCMAKE_CXX_FLAGS="-fPIC" \
              -DLLVM_ENABLE_PROJECTS=clang -DLIBCLANG_BUILD_STATIC=ON -DLLVM_ENABLE_PIC=OFF #disbale PIC because LIBCLANG_BUILD_STATIC does not work if DLLVM_ENABLE_PIC is ON
cmake --build . --target install -j 4

cd $JIYU_DIR
