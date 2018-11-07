#!/bin/sh

if [ "$#" != "1" ]; then
    echo "Illegal number of parameters, write only a number in the following format _._._"
    exit 0
fi

cd ..
./paket restore
rm -r package/Release
rm -r src/bin/Release
msbuild src/ /property:Configuration=Release 
cd package
cp -r ../src/bin/Release .

touch ./Release/.npmignore
npm version $1
npm pack .
