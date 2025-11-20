# Build script driver for the PowerShell. 
#
# Having it in addition to CMD script allows better experience in some cases,
# like interrupting the build with Ctrl+C.
#
# This was developed and tested on Windows only, though there is no reason 
# why it should not work on other platforms through PowerShell Core.

$BuildProc = Start-Process -FilePath "bazel" -ArgumentList "build", "//build_tools/cli:enso_build_cli_bin" -Wait -PassThru
if ($BuildProc.ExitCode -ne 0) {
    Write-Error "Bazel build failed."
    Exit $BuildProc.ExitCode
}

$BinPath = Join-Path $PSScriptRoot "bazel-bin" "build_tools" "cli" "enso_build_cli_bin.exe"

# We cannot use Start-Process because it doesn't attach console output properly without complex handling.
# Invoking the binary directly works better.
& $BinPath $args
if ($LASTEXITCODE -ne 0) {
    Exit $LASTEXITCODE
}
