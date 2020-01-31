#!/bin/bash

## Bundle zephyr
## Usage bundle.sh os
set -e

if [[ -z ${1} ]]; then
  echo "Usage build.sh os_name";
  exit 1;
fi


# Build OS_NAME
case $1 in
  "linux")
    OS_NAME="x86_64-linux";;
  "osx")
    OS_NAME="x86_64-osx";;
  "win64")
    OS_NAME="x86_64-windows";;
  *)
    OS_NAME=${1};;
esac

if [[ "$OS" = "win64" ]]
then
  BIN_EXT=".exe"
else
  BIN_EXT=""
fi

LOCAL_INSTALL_ROOT="dist-newstyle/build/${OS_NAME}/ghc-8.6.5/zephyr-0.2.2/x/zephyr/build/zephyr"
ZEPHYR="${LOCAL_INSTALL_ROOT}/zephyr${BIN_EXT}"

BUNDLE_DIR="bundle/zephyr"
mkdir -p ${BUNDLE_DIR}

# strip the executable
if [[ ${OS_NAME} != "x86_64-windows" ]]; then
  strip ${ZEPHYR};
fi
cp ${ZEPHYR} README.md LICENSE ${BUNDLE_DIR};

# dependencies
cabal info . > "${BUNDLE_DIR}/info"

# Calculate the SHA hash
if [[ ${OS_NAME} = "x86_64-windows" ]]; then
  SHASUM="openssl dgst -sha1";
else
  SHASUM="shasum";
fi

# Make the binary bundle
pushd bundle > /dev/null
tar -zcvf ${OS_NAME}.tar.gz zephyr

${SHASUM} ${OS_NAME}.tar.gz > ${OS_NAME}.sha
popd > /dev/null

if [[ -d ${BUNDLE_DIR} ]]; then
  rm -rf ${BUNDLE_DIR}
fi
