@echo off

pushd .\run
cl -Zi /std:c++17 /EHsc /O2 /Ob0 ..\samples\testCBackend.cpp /link -incremental:no
echo,
echo launching our sample program:
CALL ..\run\testCBackend.exe
echo program returned %errorlevel%
popd
