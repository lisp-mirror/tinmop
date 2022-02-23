## Build with containers

The directory that contains the configs and the scripts to build tinmop using containers is named 'ci', as this is usually used for a Continuous Integration or Continuous Delivery (CI/CD) infrastructure.

The bash script build.sh does all the needed stuff for you, but either podman or docker must be installed on your system and in your PATH. If both are installed, podman is preferred, as its default is to run rootless.

You have to invoke the script as PATH/TO/build.sh ARCH, for example, from the ci dir, the invocation for host arch is:

```bash
./build.sh host
```

instead, from the source dir is:

```bash
ci/build.sh host
```

This is the list of the supported archs:

- host (host architecture, autodetected)
- amd64
- i386
- armhf
- arm64
- ppc64el

For using foreign architectures (not host and supported natively, like i386 on amd64 or armhf on amd64), you must install qemu binfmt-support and qemu user static binaries first! Refer to you distribution documentation to how to setup binfmts support.
https://wiki.debian.org/QemuUserEmulation is valid for Debian, Ubuntu and derivatives, or https://github.com/multiarch/qemu-user-static for full road-to-containers solution.

Currently, riscv64 and s390x are not supported because there is no sbcl for them in Debian (and the container uses Debian as base image), and ppc64el is supported as host only (for issues with qemu user binaries).

The output of the build system (the tinmop binary), is under the out dir/ARCH/tinmop, and the default out dir is where there is the script, so in the case of the ci dir, ci/out/ARCH/tinmop .
