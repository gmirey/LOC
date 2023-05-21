@echo off

pushd .\build
cl /std:c++17 -Zi /EHsc /arch:AVX2 /Od /DALWAYS_DEFINED_TRICK /DCOMPILING_LOCLIB_DLL ..\LocLib\src\loclib.cpp /Feloclib.dll /LD /link -incremental:no
REM cl /std:c++17 -Zi /EHsc /arch:AVX2 /Od /DALWAYS_DEFINED_TRICK ..\LocLib\src\loclib.cpp /Feloclib.dll /LD /link /EXPORT:set_platform_confs /EXPORT:run_loc_compiler_from_first_file /EXPORT:release_compilation_results /EXPORT:init_report_language /EXPORT:release_report_language /EXPORT:get_filename_from_filedesc /EXPORT:get_errors_on_sourcefile /EXPORT:write_error_report_stub /EXPORT:write_error_report_desc /EXPORT:write_error_report_advice /EXPORT:get_next_line_tokens_when_reading_full_file /EXPORT:check_for_BOM /EXPORT:get_non_scan_error_info /EXPORT:text_scan_next_line /EXPORT:tokenize_line -incremental:no
REM cl /std:c++17 -Zi /EHsc /arch:AVX2 /DALWAYS_DEFINED_TRICK ..\LocLib\src\loclib.cpp /Feloclib.dll /LD /link -incremental:no
popd

