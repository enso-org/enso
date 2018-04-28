#!/bin/bash

SELF="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT=$SELF/../..

find $ROOT/core/src -name "*.hs" -print | xargs graphmod -p > $SELF/overview.dot
dot -Tpng -Gdpi=600 $SELF/overview.dot > $SELF/overview.png
