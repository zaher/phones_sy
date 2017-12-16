@echo off
del .\bin\phone_sy.exe
if errorlevel 1 goto erroroccurred

lazbuild source\phone_sy.lpr -B -q --build-mode=Release
rem if errorlevel 1 goto erroroccurred

strip ./bin/phone_sy.exe
if errorlevel 1 goto erroroccurred

set /p version=<version.txt
set /a version+=1
echo %version% > version.txt
set /p name=<name.txt
7z a phone_sy-%name%-%version%.zip version.txt bin/*.exe bin/*.dll bin/*.txt
if errorlevel 1 goto erroroccurred

goto noerrors

:erroroccurred
echo ???????????????????
echo    Error compile
echo ???????????????????
pause
goto :EOF
:noerrors
echo #######################
echo    Compile completed
echo #######################
pause
