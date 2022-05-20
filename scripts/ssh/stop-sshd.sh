#!/usr/bin/bash
set -ex
kill $(cat /home/olegat/github.com/openssh/openssh-portable/var/run/sshd.pid)
