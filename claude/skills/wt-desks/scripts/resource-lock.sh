#!/usr/bin/env bash
# Serialise a shared machine resource between agents.
#
#   LOCK_NAME=<name> [BUSY_CMD=<shell cmd>] resource-lock.sh acquire <owner>   # blocks until free
#   LOCK_NAME=<name> resource-lock.sh release <owner>
#   LOCK_NAME=<name> resource-lock.sh status
#
# BUSY_CMD, if set, must print something while the resource is in use by someone who does not
# hold the lock (e.g. a running container). acquire waits for it to print nothing as well.
set -uo pipefail
name=${LOCK_NAME:-default}
LOCK=${TMPDIR:-/tmp}/resource-lock.$name
cmd=${1:-status}
owner=${2:-unknown}

busy() { [ -n "${BUSY_CMD:-}" ] && bash -c "$BUSY_CMD" 2>/dev/null || true; }

case "$cmd" in
acquire)
    while true; do
        if [ -z "$(busy)" ] && mkdir "$LOCK" 2>/dev/null; then
            echo "$owner $(date '+%F %T') pid=$PPID" > "$LOCK/owner"
            echo "acquired $name by $owner"
            exit 0
        fi
        sleep 20
    done
    ;;
release)
    if [ -f "$LOCK/owner" ] && ! grep -q "^$owner " "$LOCK/owner"; then
        echo "$name held by $(cat "$LOCK/owner"), not $owner" >&2
        exit 1
    fi
    rm -rf "$LOCK"
    echo "released $name by $owner"
    ;;
status)
    if [ -f "$LOCK/owner" ]; then echo "held: $(cat "$LOCK/owner")"; else echo "free"; fi
    echo "busy: $(busy)"
    ;;
*)
    echo "usage: LOCK_NAME=<name> $0 acquire|release|status <owner>" >&2
    exit 2
    ;;
esac
