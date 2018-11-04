#!/bin/sh

cd ..
./paket restore
rm -r package/Release
rm -r src/bin/Release
msbuild src/ /property:Configuration=Release 
cd package
cp -r ../src/bin/Release .
cp ../packages/FSharp.Core/lib/net45/FSharp.Core.dll Release/
cp fstar.exe.config Release/

npm pack .
