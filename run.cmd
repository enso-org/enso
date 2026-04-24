@pushd %~dp0
@where bazel >nul 2>nul
@if not errorlevel 1 (
@    bazel build //build_tools/cli:enso_build_cli_bin
@    if errorlevel 1 goto end
@    bazel-bin\build_tools\cli\enso_build_cli_bin %*
) else (
@    cargo run -p enso-build-cli -- %*
)
@set EXITCODE=%ERRORLEVEL%
:end
@popd
@exit /b %EXITCODE%
