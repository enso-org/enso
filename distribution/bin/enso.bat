set comp-dir=%~dp0\..\component
java -jar -Dtruffle.class.path.append=%comp-dir%\runtime.jar -Dpolyglot.engine.IterativePartialEscape=true %JAVA_OPTS% %comp-dir%\runner.jar %*
exit /B %errorlevel%
