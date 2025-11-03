@ pushd %~dp0
cargo run --package enso-build-cli -- %*
@ popd
@ exit /b %ERRORLEVEL%
