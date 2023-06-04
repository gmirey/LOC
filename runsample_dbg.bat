@echo off

pushd .\run
cl -Zi /std:c++17 /EHsc /arch:AVX2 /Od ..\samples\testCBackend.cpp /link -incremental:no
echo,
echo launching our sample program:
CALL ..\run\testCBackend.exe
echo program returned %errorlevel%
popd
