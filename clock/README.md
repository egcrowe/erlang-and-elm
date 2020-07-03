# Preparation
Uses patches to build and install ranch, cowlib and cowboy with autotools

## ranch
```sh
patchesdir=$PWD/patches
pushd=<your/preferred/location>
git clone https://github.com/ninenines/ranch.git
pushd ranch
git am $patchesdir/ranch-adaptation-for-autotools.patch
./bootstrap.sh
./configure
make
sudo make install
popd popd
```

## cowlib
```sh
patchesdir=$PWD/patches
pushd=<your/preferred/location>
git clone https://github.com/ninenines/cowlib.git
pushd cowlib
git am $patchesdir/cowlib-adaptation-for-autotools.patch
./bootstrap.sh
./configure
make
sudo make install
popd popd
```

## cowboy
```sh
patchesdir=$PWD/patches
pushd=<your/preferred/location>
git clone https://github.com/ninenines/cowboy.git
pushd cowboy
git am $patchesdir/cowboy-adaptation-for-autotools.patch
./bootstrap.sh
./configure
make
sudo make install
popd popd
```

# Building
```sh
./bootstrap.sh
./configure
make
sudo make install
clock-start.sh
to_erl
```
