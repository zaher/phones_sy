@echo off
del .\bin\phone_sy.exe
if errorlevel 1 goto erroroccurred

lazbuild source\phone_sy.lpr -r -B -q --build-mode=Release
if errorlevel 1 goto erroroccurred

strip ./bin/phone_sy.exe
if errorlevel 1 goto erroroccurred

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
