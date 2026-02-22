@echo off

SET LAZBUILD="\usr\bin\lazbuild"
SET PROJECT="C:\Users\decro\Code\Pascal snippets\containerdemo\backup\ContainerDemo.lpi"

REM Modify .lpr file in order to avoid nothing-to-do-bug (http://lists.lazarus.freepascal.org/pipermail/lazarus/2016-February/097554.html)
echo. >> "C:\Users\decro\Code\Pascal snippets\containerdemo\backup\ContainerDemo.lpr"

%LAZBUILD% %PROJECT%

if %ERRORLEVEL% NEQ 0 GOTO END

echo. 

if "%1"=="" goto END

if /i %1%==test (
  "C:\Users\decro\Code\Pascal snippets\containerdemo\backup\ContainerDemo.exe" 
)
:END
