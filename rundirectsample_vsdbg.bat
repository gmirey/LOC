@echo off

pushd .\run
echo,
echo launching our sample program (direct x64 PE) with VS debugger:
devenv /DebugExe ..\samples\testWinX64Backend.exe
popd
