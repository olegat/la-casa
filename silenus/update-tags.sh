#!/bin/sh

# Help
USAGE_STRING=$(cat <<EOF
Usage (ctags):
  $0 tags

Usage (etags):
  $0 TAGS
EOF
)
SEE_STRING="See: $0 --help"

# Parse flags
fail="n"
help="n"
for i in "$@"; do
  case $i in
  --help)
    help="y"
    shift
    ;;
  -h)
    help="y"
    shift
    ;;
  -\?)
    help="y"
    shift
    ;;
  -*)
    printf "Unrecognized option: ${i}\n"
    fail="y"
    shift
    ;;
  # default
  *)
    ;;
  esac
done

# Process args
if [ "${help}" = y ] ; then
  echo "${USAGE_STRING}"
  exit 0
elif [ "${fail}" = y ] ; then
  printf "\n${SEE_STRING}\n"
  exit 1
fi

if [ "$1" = "tags" ] ; then
  CTAGS="ctags"
elif [ "$1" = "TAGS" ] ; then
  CTAGS="etags"
else
  printf "Missing or unknown file parameter.\n${SEE_STRING}\n"
  exit 1
fi

# Run ctags / etags
#TAGSFLAGS='--langmap=c:+.idl.l.rh,make:(Make*.in)'
pushd $(cd $(dirname $0)/.. && pwd) > /dev/null
if [ -e "$1" ] ; then
  rm "$1"
fi
echo "Updating: $(pwd)/$1"
IFS=$'\n'       # make newlines the only separator
for j in $(find -L \
  third_party \
  src \
  -name '*.c' -or \
  -name '*.h' -or \
  -name '*.m' -or \
  -name '*.cpp' -or \
  -name '*.cc' )
do
  ${CTAGS} -a ${TAGSFLAGS} "$j"
done
popd > /dev/null
