#!/bin/sh

mkdir -p dist/doc/images/overview

find src/Luna             -name '*.hs' | xargs graphmod -q --no-cluster -C Luna.Compilation -C Luna.Diagnostic -C Luna.Evaluation -C Luna.Library -C Luna.Syntax > dist/doc/images/overview/top-level.dot
find src/Luna             -name '*.hs' | xargs graphmod -q          --no-cluster                                                                                             > dist/doc/images/overview/detailed.dot
find src/Luna/Compilation -name '*.hs' | xargs graphmod -q --no-cluster                                                                                          > dist/doc/images/overview/compilation.dot
find src/Luna/Diagnostic  -name '*.hs' | xargs graphmod -q --no-cluster                                                                                          > dist/doc/images/overview/diagnostic.dot
find src/Luna/Evaluation  -name '*.hs' | xargs graphmod -q --no-cluster                                                                                          > dist/doc/images/overview/evaluation.dot
find src/Luna/Library     -name '*.hs' | xargs graphmod -q --no-cluster                                                                                          > dist/doc/images/overview/library.dot
find src/Luna/Syntax      -name '*.hs' | xargs graphmod -q --no-cluster                                                                                          > dist/doc/images/overview/syntax.dot

echo "[1/6]"
dot -Tpng -Gdpi=1000 dist/doc/images/overview/top-level.dot   > dist/doc/images/overview/top-level.png
echo "[2/6]"
dot -Tpng -Gdpi=1000 dist/doc/images/overview/detailed.dot    > dist/doc/images/overview/detailed.png
echo "[3/6]"
dot -Tpng -Gdpi=1000 dist/doc/images/overview/compilation.dot > dist/doc/images/overview/compilation.png
echo "[4/6]"
dot -Tpng -Gdpi=1000 dist/doc/images/overview/diagnostic.dot  > dist/doc/images/overview/diagnostic.png
echo "[5/6]"
dot -Tpng -Gdpi=1000 dist/doc/images/overview/evaluation.dot  > dist/doc/images/overview/evaluation.png
echo "[6/6]"
dot -Tpng -Gdpi=1000 dist/doc/images/overview/library.dot     > dist/doc/images/overview/library.png
