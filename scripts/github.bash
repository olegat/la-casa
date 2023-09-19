#!/usr/bin/env bash

open() {
    nohup open "https://github.com/ag-grid/ag-charts/commit/$1"
    cat nohup.out
    rm nohup.out
}

usage() {
    echo "github open [COMMIT]"
}

main() {
    if [[ "$1" == open ]]; then
        if [ -z "$2" ]; then
            open $(git log --format="%H" -n 1 HEAD)
        else
            open "$2"
        fi
    else
        usage
    fi
}

main "$@"
