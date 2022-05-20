#!/usr/bin/bash

# olegat@: you need to disable the firewall:
#   `sudo ufw disable`
#
# to turn it back on:
#   `sudo ufw enable`
set -ex
/home/olegat/github.com/openssh/openssh-portable/usr/sbin/sshd \
  -E /home/olegat/github.com/openssh/openssh-portable/var/log/sshd.log
