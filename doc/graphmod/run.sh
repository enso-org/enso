#!/bin/bash

SELF="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT=$SELF/../..

find $ROOT/core/src -name "*.hs" -print | xargs graphmod -p > $SELF/dist/overview.dot
dot -Tpng -Gdpi=600 $SELF/dist/overview.dot > $SELF/dist/overview.png
