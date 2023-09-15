#!/bin/bash

# nx outputs 'Error during bundle: Error: ' in front of the first error line,
# this break the next-error command for the 1st compilation error, which is
# pretty annoying. This script filters out this output and returns the exit
# status of `nx`

nx "$@" | sed -e 's/Error during bundle: Error: /\n/' ; exit "${PIPESTATUS[0]}"
