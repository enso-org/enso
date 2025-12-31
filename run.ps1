# Build script driver for the PowerShell. 
#
# Having it in addition to CMD script allows better experience in some cases,
# like interrupting the build with Ctrl+C.
#
# This was developed and tested on Windows only, though there is no reason 
# why it should not work on other platforms through PowerShell Core.

$InvokeProcess = {
    param (
        [string] $FilePath,
        [string[]] $Arguments = @(),
        [string] $WorkingDirectory = $PSScriptRoot
    )

    $psi = New-Object -TypeName System.Diagnostics.ProcessStartInfo
    $psi.FileName = $FilePath
    $psi.WorkingDirectory = $WorkingDirectory
    if ($Arguments -and $Arguments.Length -gt 0) {
        $psi.Arguments = $Arguments -join " "
    }
    $psi.UseShellExecute = $false

    $process = [System.Diagnostics.Process]::Start($psi)
    $process.WaitForExit()
    return $process.ExitCode
}

$BazelCmd = Get-Command bazel -ErrorAction SilentlyContinue
if ($BazelCmd) {
    $BuildExit = & $InvokeProcess "bazel" @("build", "//build_tools/cli:enso_build_cli_bin")
    if ($BuildExit -ne 0) {
        Write-Error "Bazel build failed."
        Exit $BuildExit
    }

    $BinPath = Join-Path $PSScriptRoot "bazel-bin" "build_tools" "cli" "enso_build_cli_bin.exe"
    $CliArgs = @()
    if ($args.Length -gt 0) {
        $CliArgs += $args
    }
    $CliExit = & $InvokeProcess $BinPath $CliArgs
    Exit $CliExit
} else {
    $CargoArgs = @("run", "-p", "enso-build-cli", "--")
    if ($args.Length -gt 0) {
        $CargoArgs += $args
    }
    $CargoExit = & $InvokeProcess "cargo" $CargoArgs
    Exit $CargoExit
}
