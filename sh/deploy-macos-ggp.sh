#!/bin/bash

TARFILE=${HOME}/Downloads/macosSDK.tar.gz
YETISDK=${HOME}/YetiSDK

# Download
scp olegat@olegat-linux.c.googlers.com:/tmp/build_macos/macosSDK.tar.gz ${TARFILE}

# Clean
rm -fr    ${YETISDK}
mkdir -p  ${YETISDK}

# Extract
pushd ${YETISDK} > /dev/null
tar -xzf ${TARFILE}
popd > /dev/null