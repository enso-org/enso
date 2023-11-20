@ pushd %~dp0
cargo run --profile buildscript --package enso-build-cli -- %*
@ popd
@ exit /b %ERRORLEVEL%
