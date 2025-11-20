@ pushd %~dp0
@ bazel build //build_tools/cli:enso_build_cli_bin
@ bazel-bin\build_tools\cli\enso_build_cli_bin %*
@ popd
@ exit /b %ERRORLEVEL%
