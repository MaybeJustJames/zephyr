#!/bin/bash

## Bundle zephyr
## Usage bundle.sh os
set -e


if [[ -z ${1} ]]; then
  echo "Usage build.sh [Linux|macOS|Windows]";
  exit 1;
fi

OS_NAME="$1"

if [[ "$OS" = "Windows" ]]
then
  BIN_EXT=".exe"
else
  BIN_EXT=""
fi

BUNDLE_DIR="bundle/zephyr"
mkdir -p ${BUNDLE_DIR}
ZEPHYR="${BUNDLE_DIR}/zephyr${BIN_EXT}"
cabal install --install-method=copy --installdir ${BUNDLE_DIR} exe:zephyr

# strip the executable
if [[ ${OS_NAME} != "Windows" && -a ${ZEPHYR} ]]; then
  strip ${ZEPHYR};
fi
cp README.md LICENSE ${BUNDLE_DIR};

# dependencies
cabal info . > "${BUNDLE_DIR}/info"

# Calculate the SHA hash
if [[ ${OS_NAME} = "Windows" ]]; then
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
