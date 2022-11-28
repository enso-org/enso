# Build script driver for the PowerShell. 
#
# Having it in addition to CMD script allows better experience in some cases,
# like interrupting the build with Ctrl+C.
#
# This was developed and tested on Windows only, though there is no reason 
# why it should not work on other platforms through PowerShell Core.
$ErrorActionPreference = "Stop"
$TargetDir = Join-Path $PSScriptRoot "target" "enso-build"
$BuildScriptProfile = "buildscript"
$BuildScriptBin = "enso-build-cli"

$TargetExe = Join-Path $TargetDir $BuildScriptProfile $BuildScriptBin

$BuildArgs = "build", "--profile", $BuildScriptProfile, "--target-dir", $TargetDir, "--package", $BuildScriptBin
Set-Location $PSScriptRoot
Start-Process cargo -NoNewWindow -Wait -ArgumentList $BuildArgs
if (!$?) { Exit $LASTEXITCODE }
Start-Process $TargetExe -NoNewWindow -Wait -ArgumentList $args 
Exit $LASTEXITCODE
