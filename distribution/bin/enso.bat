@echo off
set comp-dir=%~dp0\..\component
set EXTRA_OPTS=-Dgraal.PrintGraph=Network
FOR %%A in (%*) DO (
if /I %%A==--dump-graphs (
set EXTRA_OPTS=%EXTRA_OPTS% -Dgraal.Dump=Truffle:1
)
)
set JAVA_OPTS=%JAVA_OPTS --add-opens=java.base/java.nio=ALL-UNNAMED
java --module-path %comp-dir% -Dorg.graalvm.language.enso.home=%comp-dir% -Dpolyglot.compiler.IterativePartialEscape=true %EXTRA_OPTS% %JAVA_OPTS% -m org.enso.runtime/org.enso.EngineRunnerBootLoader %*
exit /B %errorlevel%
