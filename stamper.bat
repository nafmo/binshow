@echo off
if "%1"=="" goto nopar
if not %1==/? goto ok
echo STAMP-IT mm-dd-yyyy vv:vv
echo   ex: STAMP-IT 08-13-1993 02:00
goto slut
:nopar
echo parameter saknas
goto slut
:ok
call stamp binshow.exe %1 %2:00
call stamp binshow.do? %1 %2:00
call stamp file_id.diz %1 %2:00
:slut
