#!/bin/sh
# curl-wget.sh - wget substitute using curl

usage() {
    echo "(wget substitute implemented in curl)" >&2
    echo "" >&2
    echo "Usage: $0 [curl options] URL" >&2
    echo "Example: $0 -v http://example.com/robots.txt" >&2
    exit 1
}

get_basename() {
    last="$1"
    shift
    for arg; do
        last="$arg"
    done
    basename "$last"
}

[ $# -eq 0 ] && usage
curl -L -o "$(get_basename "$@")" "$@"
