set comp-dir=%~dp0\..\component
java -jar -Dpolyglot.engine.IterativePartialEscape=true -XX:-UseJVMCIClassLoader -Dpolyglot.engine.BackgroundCompilation=false -Dgraalvm.locatorDisabled=true %JAVA_OPTS% %comp-dir%\enso.jar %*
exit /B %errorlevel%
