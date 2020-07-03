# Introduction
The repository is intended to provide self contained examples for
building, installing and running web server applications that serve
web pages rendered using the `elm` programming language. However it
also exemplifies a build system methodology for managing build and
runtime dependencies, itself a complex and opinionated subject matter.

# Building Erlang/OTP
Build Erlang/OTP is relatively easy. You just need to ensure that the
Linux Distro you are using has the necessary dependencies installed,
particularly `ncurses` and `openssl`:
```sh
git clone https://github.com/erlang/otp.git
pushd otp
./otp_build autoconf
./configure
make -j4
sudo make install
```

# Building Additional Dependent Applications
A webserver is needed to serve HTML pages embedded with `elm`
scripts. A popular webserver developed in Erlang is *cowboy* which has
runtime dependencies to rge Erlang applications *cowlib* and
*ranch*. It actually has more dependencies to other applications to
fully support running tests and generating the documentation,
applications such as *Concuerror*, *ct_helper*, *gun*, *horse*,
*proper* and *stampede*.

These instructions focus on building and installing *ranch*, *cowlib*
and *cowboy* applying patches to these project in order to provide a
GNU compliant build system. The applications are installed alongside
the Erlang/OTP applications such that the erlang code loader can
easily locate them (no need to fiddle around with setting
$ERL_LIB). These allows for other application build systems to locate
(or not) application dependencies they have. These instructions assume
that git repositories are cloned to $HOME.
```sh
cd $HOME
git clone
https://github.com/egcrowe/erlang-and-helm.git
```

## ranch
```sh
git clone https://github.com/ninenines/ranch.git
pushd ranch
git checkout 2.0.0
git am $HOME/erlang-and-elm/patches/ranch-adaptation-for-autotools.patch
./bootstrap.sh
./configure
make
sudo make install
popd
```

## cowlib
```sh
git clone https://github.com/ninenines/cowlib.git
pushd cowlib
git checkout 2.9.1
git am $HOME/erlang-and-elm/patches/cowlib-adaptation-for-autotools.patch
./bootstrap.sh
./configure
make
sudo make install
popd
```

## cowboy
```sh
git clone https://github.com/ninenines/cowboy.git
pushd cowboy
git checkout 2.8.0
git am $HOME/erlang-and-elm/patches/cowboy-adaptation-for-autotools.patch
./bootstrap.sh
./configure
make
sudo make install
popd
```

# Examples
This repository contains three examples in their respective directories:
1. hello    [html with embedded javascript]
2. clock    [html with embedded javascript]
3. clock_js [html with reference to javascript]

Goto each directory for specific build, install and run instructions.
