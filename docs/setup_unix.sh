
set -ex

JIYU_DIR=$PWD

LLVM_VERSION=8.0.0
LLVM_URL=http://releases.llvm.org/8.0.0/llvm-$LLVM_VERSION.src.tar.xz
CLANG_URL=http://releases.llvm.org/8.0.0/cfe-$LLVM_VERSION.src.tar.xz

if [ -z $LLVM_TARGETS ]
then
    LLVM_TARGETS="X86;ARM;AArch64"
fi

if [[ ! -f llvm-$LLVM_VERSION.src.tar.xz ]]
then
    wget $LLVM_URL || curl -L --output llvm-$LLVM_VERSION.src.tar.xz $LLVM_URL
fi

if [[ ! -f cfe-$LLVM_VERSION.src.tar.xz ]]
then
    wget $CLANG_URL || curl -L --output cfe-$LLVM_VERSION.src.tar.xz $CLANG_URL
fi

if [[ ! -d llvm-$LLVM_VERSION.src ]]
then
    tar -vxzf llvm-$LLVM_VERSION.src.tar.xz
fi

if [[ ! -d llvm-$LLVM_VERSION.src/tools/clang ]]
then
    tar -vxzf cfe-$LLVM_VERSION.src.tar.xz
    mv cfe-$LLVM_VERSION.src llvm-$LLVM_VERSION.src/tools/clang
fi

cd llvm-$LLVM_VERSION.src
mkdir -p build
cd build

cmake .. -DCMAKE_BUILD_TYPE=RelWithDebInfo -DLLVM_TARGETS_TO_BUILD=$LLVM_TARGETS -DLLVM_ENABLE_DUMP=ON -DCMAKE_INSTALL_PREFIX=$JIYU_DIR/llvm
cmake --build . --target install

cd $JIYU_DIR