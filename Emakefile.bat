@echo off

set EPWD_DIR=%cd%

set ELOG_DIR=%EPWD_DIR%\eLog

set EBIN_DIR=%EPWD_DIR%\eBin

set CTCP_DIR=%EBIN_DIR%\core\tcp

set CUDP_DIR=%EBIN_DIR%\core\udp

set STCP_DIR=%EBIN_DIR%\spec\tcp

set SUDP_DIR=%EBIN_DIR%\spec\udp

echo Initialize the compile environment

if not exist %ELOG_DIR% md %ELOG_DIR%

if not exist %EBIN_DIR% md %EBIN_DIR%

if not exist %CTCP_DIR% md %CTCP_DIR%

if not exist %CUDP_DIR% md %CUDP_DIR%

if not exist %STCP_DIR% md %STCP_DIR%

if not exist %SUDP_DIR% md %SUDP_DIR%

for %%f in (%cd%\eLib\mod4erl\eSrc\*.erl) do erlc -o %cd%\eLib\mod4erl\eBin %%f

echo Compile the source code

erl -pa ./eLib/mod4erl/eBin -make