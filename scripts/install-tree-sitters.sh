#!/bin/sh
set -ex

git clone https://github.com/tree-sitter/tree-sitter-typescript

cd tree-sitter-typescript/typescript
cc -fPIC -I./src -shared src/parser.c src/scanner.c -o libtree-sitter-typescript.dylib
mv libtree-sitter-typescript.dylib ~/.emacs.d/tree-sitter/

cd ../tsx
cc -fPIC -I./src -shared src/parser.c src/scanner.c -o libtree-sitter-tsx.dylib
mv libtree-sitter-tsx.dylib ~/.emacs.d/tree-sitter/
