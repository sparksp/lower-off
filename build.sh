#!/bin/bash
if [ -z $URL ]; then echo "ERROR: No URL set"; exit 1; fi
ROOT=$(dirname $BASH_SOURCE)
mkdir -p $ROOT/dist

# API
hugo -b $URL/ -d $ROOT/dist/api/ -s $ROOT/api/

# WEB
( cd $ROOT/web && npm i && npm run build )
cp -a $ROOT/web/dist/ $ROOT/dist/
