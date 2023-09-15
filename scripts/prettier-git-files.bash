#!/bin/bash

if [[ "${1}" == "--run" ]] ; then
    ECHO=""
else
    ECHO="echo" # dry run
fi

git status --porcelain | \
    egrep '^[MA ][MA ] ' | \
    cut -c 3- | \
    xargs -I {} ${ECHO} \
          ~/ag-charts/node_modules/.bin/prettier --config ~/ag-charts/.prettierrc -w {}
