@echo off

pushd .\build
cl /std:c++17 -Zi /EHsc /arch:AVX2 /Od /DMANUALLY_USING_LOCLIB_DLL ..\win32_LocCmd\src\win32_loccmd.cpp /Feloc.exe /link user32.lib -incremental:no
popd