#!/bin/bash
if [ -z $URL ]; then echo "ERROR: No URL set"; exit 1; fi
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
mkdir -p $DIR/dist

# API
hugo -b $URL/ -d $DIR/dist/api/ -s $DIR/api/

# WEB
( cd $DIR/web && npm i && npm run build )
cp -Rv $DIR/web/dist/ $DIR/dist/
