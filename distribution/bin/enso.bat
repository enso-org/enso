@echo off
set comp-dir=%~dp0\..\component
set EXTRA_OPTS=-Dgraal.PrintGraph=Network
FOR %%A in (%*) DO (
if /I %%A==--dump-graphs (
set EXTRA_OPTS=%EXTRA_OPTS% -Dgraal.Dump=Truffle:1
)
)
java -jar -Dtruffle.class.path.append=%comp-dir%\runtime.jar -Dpolyglot.engine.IterativePartialEscape=true %EXTRA_OPTS% %JAVA_OPTS% %comp-dir%\runner.jar %*
exit /B %errorlevel%
