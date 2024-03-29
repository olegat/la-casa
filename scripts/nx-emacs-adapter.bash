#!/bin/bash

# nx outputs 'Error during bundle: Error: ' in front of the first error line,
# this break the next-error command for the 1st compilation error, which is
# pretty annoying. This script filters out this output and returns the exit
# status of `nx`

export NVM_DIR="$HOME/.nvm"
[ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

nx "$@" | sed -e 's/Error during bundle: Error: /\n/' ; exit "${PIPESTATUS[0]}"
