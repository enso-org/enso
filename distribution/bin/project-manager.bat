set comp-dir=%~d0\..\component
java -jar -Dtruffle.class.path.append=%comp-dir%\runtime.jar %JAVA_OPTS% %comp-dir%\project-manager.jar %*
exit /B %errorlevel%
