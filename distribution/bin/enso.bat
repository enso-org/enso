@echo off
set comp-dir=%~dp0\..\component
set java-opts=--enable-native-access=org.graalvm.truffle --sun-misc-unsafe-memory-access=allow --add-opens=java.base/java.nio=ALL-UNNAMED
java --module-path %comp-dir%  %java-opts% -m org.enso.runner/org.enso.runner.Main %*
exit /B %errorlevel%
