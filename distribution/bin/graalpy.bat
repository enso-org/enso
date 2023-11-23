@echo off
set comp-dir=%~dp0\..\component
java --module-path %comp-dir% -Dpolyglot.compiler.IterativePartialEscape=true %EXTRA_OPTS% %JAVA_OPTS% -m org.graalvm.py.launcher/com.oracle.graal.python.shell.GraalPythonMain %*
exit /B %errorlevel%
