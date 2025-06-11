@echo off
set comp-dir=%~dp0\..\component
set _JAVA_OPTIONS=%_JAVA_OPTIONS% --enable-native-access=org.graalvm.truffle --sun-misc-unsafe-memory-access=allow --add-opens=java.base/java.nio=ALL-UNNAMED
java --module-path %comp-dir% -Dpolyglot.compiler.IterativePartialEscape=true %_JAVA_OPTIONS% -m org.enso.runner/org.enso.runner.Main %*
exit /B %errorlevel%
