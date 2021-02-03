# -*- Mode: sh; sh-indentation: 2; -*-

emacs() {
  # TODO: $@ will not work for spaces and fullpaths.
  #       It should use "" and `cygpath -w` but good enough for now.
  cmd /c "cmd /c $EMACS_ROOT\\bin\\runemacs.exe $@"
}
