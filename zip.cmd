set /p version=<version.txt
set /a version+=1
echo %version% > version.txt
set /p name=<name.txt
7z a phone_sy-name-%version%.zip version.txt bin/*.exe bin/*.dll bin/*.txt bin/*.sql 
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

