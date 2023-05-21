@echo off
cls

echo ---------------------------------
echo ---------------------------------
echo Building Lib:
echo ---------------------------------
call ./build_libonly.bat

echo,
echo ---------------------------------
echo ---------------------------------
echo Building Win32 Command-Line Exe:
echo ---------------------------------
call ./build_win32.bat
