
set -ex

LLVM_VERSION=8.0.0

# From this stackoverflow answer from paxdiablo https://stackoverflow.com/a/3466183
unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     machine=Linux;;
    Darwin*)    machine=Mac;;
    CYGWIN*)    machine=Cygwin;;
    MINGW*)     machine=MinGw;;
    *)          machine="UNKNOWN:${unameOut}"
esac
echo ${machine}

if [ $machine == "Mac" ]
then
    if [ ! -f llvm-prebuilt.tar.xz ]
    then
        LLVM_URL="http://releases.llvm.org/$LLVM_VERSION/clang+llvm-$LLVM_VERSION-x86_64-apple-darwin.tar.xz"
        curl -L --output llvm-prebuilt.tar.xz $LLVM_URL
    fi
    tar -xzf llvm-prebuilt.tar.xz
    mv "clang+llvm-$LLVM_VERSION-x86_64-apple-darwin" llvm
fi


if [ $machine == "Linux" ]
then
    if [ ! -f llvm-prebuilt.tar.xz ]
    then
        if [ $(lsb_release -d) == "Ubuntu" ]
        then
            UBUNTU_VERSION=$(lsb_release -rs)
            LLVM_URL="http://releases.llvm.org/$LLVM_VERSION/clang+llvm-$LLVM_VERSION-x86_64-linux-gnu-ubuntu-$UBUNTU_VERSION.tar.xz"
            wget -O llvm-prebuilt.tar.xz $LLVM_URL || curl -L --output llvm-prebuilt.tar.xz $LLVM_URL
        fi
    fi

    if [ ! -f llvm-prebuilt.tar.xz ]
    then
        echo "Could not download a prebuilt LLVM tarball for this platform, please use ./docs/setup_unix.sh instead."
        exit
    fi

    tar -xzf llvm-prebuilt.tar.xz
    mv "clang+llvm-$LLVM_VERSION-x86_64-linux-gnu-ubuntu-$UBUNTU_VERSION" llvm
fi