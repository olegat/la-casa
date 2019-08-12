#!/bin/bash

#------------------------------------------------------------------------------
# Variables
#------------------------------------------------------------------------------
#TMPDIR=/tmp/build_macos_$(cat /dev/urandom | tr -dc 'a-zA-Z0-9' | head --bytes=10)
TMPDIR=/tmp/build_macos # fixed path makes downloading files easier
GOOGLE3=/google/src/cloud/olegat/yeti/google3
OUTPUT_NAME=macosSDK.tar.gz



#------------------------------------------------------------------------------
# Authenticate
#------------------------------------------------------------------------------
echo 'Running prodaccess...'
prodaccess



#------------------------------------------------------------------------------
# Build
#------------------------------------------------------------------------------
# Run blaze commands.
pushd "${GOOGLE3}" > /dev/null
blaze build --config=darwin_x86_64 chrome/cloudcast/tools/cli:ggp_darwin
blaze build --config=darwin_x86_64 chrome/cloudcast/devchromefe/chrome_client:chrome_client_darwin
popd > /dev/null


#------------------------------------------------------------------------------
# Package
#------------------------------------------------------------------------------
# Rename / Re-structure hierarchy.
rm -fr "${TMPDIR}"
mkdir -p "/${TMPDIR}/sdk/dev/bin/chrome_client"
cp "${GOOGLE3}/blaze-bin/chrome/cloudcast/tools/cli/ggp_darwin" \
  "${TMPDIR}/sdk/dev/bin/ggp"

cp "${GOOGLE3}/blaze-bin/chrome/cloudcast/devchromefe/chrome_client/chrome_client_darwin" \
  "${TMPDIR}/sdk/dev/bin/chrome_client/chrome_client"

# Create tarball
pushd "/${TMPDIR}/sdk" > /dev/null
tar -czf ../${OUTPUT_NAME} *
popd > /dev/null

echo "Ouput file: ${TMPDIR}/${OUTPUT_NAME}"