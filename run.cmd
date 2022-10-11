pushd %~dp0
@ set TARGET_DIR=%~dp0target\enso-build
@ set TARGET_EXE=%TARGET_DIR%\buildscript\enso-build-cli.exe
cargo build --profile buildscript --target-dir "%TARGET_DIR%" --package enso-build-cli && "%TARGET_EXE%" %*
popd
exit /b %ERRORLEVEL%
