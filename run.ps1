# Build script driver for the PowerShell. 
#
# Having it in addition to CMD script allows better experience in some cases,
# like interrupting the build with Ctrl+C.
#
# This was developed and tested on Windows only, though there is no reason 
# why it should not work on other platforms through PowerShell Core.
$ErrorActionPreference = "Stop"
$TargetDir = Join-Path $PSScriptRoot "target" "enso-build"
$TargetExe = Join-Path $TargetDir "buildscript" "enso-build-cli"

$BuildArgs = "build-cli", "--target-dir", $TargetDir
$BuildScriptProcess = Start-Process cargo -NoNewWindow -PassThru -Wait -WorkingDirectory $PSScriptRoot -ArgumentList $BuildArgs
if ($BuildScriptProcess.ExitCode -ne 0) {
    Exit $BuildScriptProcess.ExitCode
}
$BuildScriptBinProcess = Start-Process $TargetExe -NoNewWindow -PassThru -Wait -WorkingDirectory $PSScriptRoot -ArgumentList $args
Exit $BuildScriptBinProcess.ExitCode
