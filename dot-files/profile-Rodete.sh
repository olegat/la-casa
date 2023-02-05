#!/bin/sh

#------------------------------------------------------------------------------
# Command line colors
#------------------------------------------------------------------------------
# TODO(olegat) improve PWD: -20
export PS1='\[\033[01;32m\]\h\[\033[00m\]:\[\033[01;34m\]..${PWD: -20}\[\033[00m\]\$ '
export P4DIFF="diff --color -up"



#------------------------------------------------------------------------------
# Local tools
#------------------------------------------------------------------------------
# Vulkan SDK
export VULKAN_SDK="/usr/local/google/home/olegat/VulkanSDK/1.2.141.2/x86_64"

# pip path
export PATH="/usr/local/google/home/olegat/.local/bin:${PATH}"

# Tools built manually 'e.g. configure --prefix= ; make install'
export OLEGAT_PREFIX="/usr/local/google/home/olegat/usr"
export PATH="${PATH}:${OLEGAT_PREFIX}/bin"

if [ -f /google/bin/releases/copybara/public/copybara/copybara ] ; then
  alias copybara='/google/bin/releases/copybara/public/copybara/copybara'
fi

alias gclfmt="/google/data/ro/projects/borg/gclfmt"
