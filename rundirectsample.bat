@echo off

pushd .\run
echo,
echo launching our sample program (direct x64 PE):
CALL ..\samples\testWinX64Backend.exe
echo program returned %errorlevel%
popd
