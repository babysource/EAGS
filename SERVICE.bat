@echo off

if "%OS%" == "Windows_NT" setlocal

if "%1" == "" goto displayUsage

cd /d %~dp0

set ERLANG_NODE=
set SOCKET_MODE=
set SOCKET_PORT=
set SOCKET_AGRT=

set "ACTION_TYPE=%1" && shift

:analyseParam
if not "%1" == "" (
	if "%1" == "/node" goto analyse_node
	if "%1" == "-node" goto analyse_node
	if "%1" == "/mode" goto analyse_mode
	if "%1" == "-mode" goto analyse_mode
	if "%1" == "/agrt" goto analyse_agrt
	if "%1" == "-agrt" goto analyse_agrt
	if "%1" == "/port" goto analyse_port
	if "%1" == "-port" goto analyse_port
)
goto serviceCheck

:analyse_next
shift && shift
goto analyseParam

:analyse_node
set ERLANG_NODE=%2
goto analyse_next

:analyse_mode
set SOCKET_MODE=%2
goto analyse_next

:analyse_agrt
set SOCKET_AGRT=%2
goto analyse_next

:analyse_port
set SOCKET_PORT=%2
goto analyse_next

:serviceCheck
if %SOCKET_MODE% == tcp set "SOCKET_TYPE=TCP" && goto serivceApply
if %SOCKET_MODE% == udp set "SOCKET_TYPE=UDP" && goto serivceApply
goto displayUsage

:serivceApply
if "%SOCKET_PORT%" == "" goto displayUsage

echo Using EAGS_HOME: "%CD%"
echo Using ERTS_HOME: "%ERTS_HOME%"

rem Set EXECUTE file
set EXECUTE_FILE="%ERTS_HOME%"\bin\erlsrv.exe

rem Set Display name
set DISPLAY_NAME="EAGS_%SOCKET_TYPE%_%SOCKET_PORT%"

rem Set Service name
set SERVICE_NAME="EAGSvc$%SOCKET_MODE%%SOCKET_PORT%"

if %ACTION_TYPE% == install goto doInstall
if %ACTION_TYPE% == destroy goto doDestroy
echo.
echo Unknown parameter "%ACTION_TYPE%"

:displayUsage
echo.
echo Usage: service.bat install/destroy [/node node] [/mode mode] [/agrt agrt]  [/port port]
echo.
echo        /node
echo               Only effective when installed. Set a node name for the erlang machine.
echo.
echo        /mode
echo               Set the socket mode. tcp or udp.
echo.
echo        /agrt
echo               Only effective when installed. Set the socket agreement.
echo.
echo        /port
echo               Set the socket port.
goto end

:doDestroy
rem Destroy the service
echo Destroying the service "%SERVICE_NAME%" ...
echo.

%EXECUTE_FILE% remove %DISPLAY_NAME%

if not errorlevel 1 goto destroyed
echo Failed destroying "%SERVICE_NAME%" service
goto end

:doInstall
if "%ERLANG_NODE%" == "" goto displayUsage
if "%SOCKET_AGRT%" == "" goto displayUsage

rem Install the service
echo Installing the service "%SERVICE_NAME%" ...
echo.

%EXECUTE_FILE% add %DISPLAY_NAME% -i %SERVICE_NAME% -c %ERLANG_NODE%(%SOCKET_AGRT%) -w %CD%\ebin -d reuse -sn %ERLANG_NODE% -ar "-run ags launch_%SOCKET_MODE% %SOCKET_AGRT% %SOCKET_PORT%" -on restart_always

if not errorlevel 1 goto installed
echo Failed installing "%SERVICE_NAME%" service
goto end

:installed
echo The service "%SERVICE_NAME%" has been Installed
echo.
goto run

:destroyed
echo The service "%SERVICE_NAME%" has been Destroyed
goto end

:started
echo The service "%SERVICE_NAME%" has been Started
goto end

:run
rem Start the service
echo Starting the service "%SERVICE_NAME%" ...
echo.

%EXECUTE_FILE% start %DISPLAY_NAME%

if not errorlevel 1 goto started
echo Failed Starting "%SERVICE_NAME%" service

:end
endlocal