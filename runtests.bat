@echo off

cls

pushd .\build
cl /std:c++17 -Zi /EHsc /arch:AVX2 /Od /DMANUALLY_USING_LOCLIB_DLL ..\win32_LocTests\src\win32_loctests.cpp /Feloctest.exe /link user32.lib -incremental:no
popd

pushd .\tests
CALL ..\build\loctest.exe
popd