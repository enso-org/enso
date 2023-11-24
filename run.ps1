# Build script driver for the PowerShell. 
#
# Having it in addition to CMD script allows better experience in some cases,
# like interrupting the build with Ctrl+C.
#
# This was developed and tested on Windows only, though there is no reason 
# why it should not work on other platforms through PowerShell Core.

$RunArgs = @("run", "--profile", "buildscript", "--package",  "enso-build-cli", "--")
$RunArgs += $args

$psi = New-Object -TypeName System.Diagnostics.ProcessStartInfo -ArgumentList "cargo",$RunArgs
$psi.WorkingDirectory = $PSScriptRoot
$handle = [System.Diagnostics.Process]::Start($psi)
$handle.WaitForExit()
Exit $handle.ExitCode
