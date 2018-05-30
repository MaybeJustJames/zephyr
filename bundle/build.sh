## Bundle zephyr
## Usage bundle.sh os
set -e

nix="";
if [[ ${1} == "--nix" ]]; then
  nix="--nix";
  shift;
fi

if [[ -z ${1} ]]; then
  echo "Usage build.sh os_name";
  exit 1;
fi


# Build OS_NAME
case $1 in
  "linux")
    OS_NAME="linux64";;
  "osx")
    OS_NAME="macos";;
  *)
    OS_NAME=${1};;
esac

pushd $(stack path --project-root);

if [[ "$OS" = "win64" ]]
then
  BIN_EXT=".exe"
else
  BIN_EXT=""
fi
LOCAL_INSTALL_ROOT=$(stack path ${nix} --local-install-root)

BUNDLE_DIR="bundle/zephyr"
mkdir -p ${BUNDLE_DIR}

ZEPHYR_BIN="${LOCAL_INSTALL_ROOT}/bin/zephyr${BIN_EXT}"
# strip the executable
if [[ ${OS_NAME} != "win64" ]]; then
  strip ${ZEPHYR_BIN};
fi
cp ${ZEPHYR_BIN} README.md LICENSE ${BUNDLE_DIR};

# dependencies
stack ${nix} ls dependencies > "${BUNDLE_DIR}/dependencies"

# Calculate the SHA hash
if [[ ${OS_NAME} = "win64" ]]; then
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
