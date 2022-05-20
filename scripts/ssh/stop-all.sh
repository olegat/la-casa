#!/usr/bin/bash
set -ex
kill $(ps -xc | grep sshd | awk '{print $1}')
