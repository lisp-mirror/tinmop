#!/usr/bin/env bash

set -e

podman_bin=`command -v podman`
docker_bin=`command -v docker`
SCRIPT_DIR=$(cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd)
SRC_DIR=$(cd $SCRIPT_DIR/.. && pwd)
OUTDIR="$SCRIPT_DIR/out"
ARCH=$1

mkdir -p $OUTDIR
cd $SCRIPT_DIR

build() {
    [[ -z "$($CT_RUNTIME images -q tinmop_ci:$CT_TAG)" ]] || $CT_RUNTIME rmi tinmop_ci:$CT_TAG
    $CT_RUNTIME build -t tinmop_ci:$CT_TAG -f Containerfile --build-arg ARCH=$CT_ARCH .
    $CT_RUNTIME run --rm -v $SRC_DIR:/src/tinmop -v $OUTDIR:/src/out --name tinmop_ci $($CT_RUNTIME image ls -q tinmop_ci:$CT_TAG)
    $CT_RUNTIME rmi tinmop_ci:$CT_TAG
}

check_user_qemu() {
    if [[ ! -f /proc/sys/fs/binfmt_misc/qemu-$USER_ARCH ]]
    then
       echo "You must have qemu user static for $USER_ARCH and binfmt support enabled for this arch!"
       exit 1
    fi
}

unsupported_arch() {
    echo "Currently, you can not build for $ARCH via container.
Exiting..."
    exit 0
}

unsupported_user_arch() {
    echo "Currently, you can not build for $ARCH via qemu user static binary for issues with it.
Please try again, using an $ARCH host.
Exiting..."
    exit 0
}

if [[ -x "${podman_bin}" ]]
then
    echo 'Using podman...'
    CT_RUNTIME=$podman_bin
elif [[ -x "${docker_bin}" ]] && [[ ${EUID} -eq 0 ]]
then
    echo 'Using docker...'
    CT_RUNTIME=$docker_bin
elif [[ -x "${docker_bin}" ]] && [[ ${EUID} -ne 0 ]]
then
    echo "By default, docker do not run rootless.
You need to run $0 as root or via sudo."
else
    echo 'You need to install podman or docker first!
See your distro docs to install one of them.'
    exit 1
fi

# We are using Debian base image, so we can pass only valid archs from the base imge
# Mapping architecture names

if [[ $ARCH == 'host' ]]
then
    if [[ $(uname -m) == 'x86_64' ]]
    then
        CT_ARCH='amd64'
        CT_TAG='amd64'
    elif [[ $(uname -m) == 'i386' ]]
    then
        CT_ARCH='i386'
        CT_TAG='i386'
    elif [[ $(uname -m) == 'aarch64' ]]
    then
        CT_ARCH='arm64v8'
        CT_TAG='arm64'
    elif [[ $(uname -m) == 'armv7l' ]]
    then
        CT_ARCH='arm32v7'
        CT_TAG='armhf'
    elif [[ $(uname -m) == 'ppc64le' ]]
    then
        CT_ARCH='ppc64le'
        CT_TAG='ppc64el'
    elif [[ $(uname -m) == 's390x' ]]
    then
        CT_ARCH='s390x'
        CT_TAG='s390x'
        unsupported_arch
    elif [[ $(uname -m) == 'riscv64' ]]
    then
        CT_ARCH='riscv64'
        CT_TAG='riscv64'
        unsupported_arch
    else
        echo 'Sorry, your host architecture is not supported!
Exiting...'
        exit 1
    fi
elif [[ $ARCH == 'amd64' ]]
then
    CT_ARCH='amd64'
    CT_TAG=$ARCH
    if [[ $(uname -m) != 'x86_64' ]]
    then
        USER_ARCH='x86_64'
        check_user_qemu
    fi
elif [[ $ARCH == 'i386' ]]
then
    CT_ARCH='i386'
    CT_TAG=$ARCH
    if [[ $(uname -m) != 'i386' ]] && [[ $(uname -m) != 'x86_64' ]]
    then
        USER_ARCH='i386'
        check_user_qemu
    fi
elif [[ $ARCH == 'arm64' ]]
then
    CT_ARCH='arm64v8'
    CT_TAG=$ARCH
    if [[ $(uname -m) != 'aarch64' ]]
    then
        USER_ARCH='aarch64'
        check_user_qemu
    fi
elif [[ $ARCH == 'armhf' ]]
then
    CT_ARCH='arm32v7'
    CT_TAG=$ARCH
    if [[ $(uname -m) != 'armv7l' ]] && [[ $(uname -m) != 'aarch64' ]]
    then
        USER_ARCH='arm'
        check_user_qemu
    fi
elif [[ $ARCH == 'ppc64el' ]]
then
    CT_ARCH='ppc64le'
    CT_TAG=$ARCH
    unsupported_user_arch
    if [[ $(uname -m) != 'ppc64le' ]]
    then
        USER_ARCH='ppc64le'
        check_user_qemu
    fi
elif [[ $ARCH == 's390x' ]]
then
    CT_ARCH='s390x'
    CT_TAG=$ARCH
    unsupported_arch
    if [[ $(uname -m) != 's390x' ]]
    then
        USER_ARCH='s390x'
        check_user_qemu
    fi
elif [[ $ARCH == 'riscv64' ]]
then
    CT_ARCH='riscv64'
    CT_TAG=$ARCH
    unsupported_arch
    if [[ $(uname -m) != 'riscv64' ]]
    then
        USER_ARCH='riscv64'
        check_user_qemu
    fi
else
    echo 'Please specific a valid architecture!'
    exit 1
fi

echo "Building for $ARCH architecture..."
build
echo "All done! You can find executable at:
$SCRIPT_DIR/out/$ARCH/tinmop"
exit 0
