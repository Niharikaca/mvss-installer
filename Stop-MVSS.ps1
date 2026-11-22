$ErrorActionPreference = 'Stop'

$installRoot = $PSScriptRoot
$expectedRuntime = [System.IO.Path]::GetFullPath((Join-Path $installRoot 'R-portable\bin\Rscript.exe'))
$expectedRunner = [System.IO.Path]::GetFullPath((Join-Path $installRoot 'Launch-MVSS.R'))
$pidFile = Join-Path (Join-Path $env:LOCALAPPDATA 'MVSS') 'server.pid'

function Show-StopMessage([string]$Text, [bool]$IsError = $false) {
    if ($env:MVSS_TEST_MODE -eq '1') { Write-Host $Text; return }
    Add-Type -AssemblyName System.Windows.Forms
    $icon = if ($IsError) { [System.Windows.Forms.MessageBoxIcon]::Warning } else { [System.Windows.Forms.MessageBoxIcon]::Information }
    [System.Windows.Forms.MessageBox]::Show($Text, 'MVSS', [System.Windows.Forms.MessageBoxButtons]::OK, $icon) | Out-Null
}

try {
    # Identify only Rscript processes launched with this installation's exact
    # MVSS runner path. This also handles sessions left by older releases.
    $matchingProcesses = @(Get-CimInstance Win32_Process -Filter "Name = 'Rscript.exe'" |
        Where-Object {
            ([string]$_.ExecutablePath -ieq $expectedRuntime) -and
            ([string]$_.CommandLine).IndexOf($expectedRunner, [System.StringComparison]::OrdinalIgnoreCase) -ge 0
        })

    if ($matchingProcesses.Count -eq 0) {
        Remove-Item -LiteralPath $pidFile -Force -ErrorAction SilentlyContinue
        Show-StopMessage 'MVSS is already closed.'
        exit 0
    }

    # Match the actual executable and runner path before stopping anything.
    # This also recovers sessions left by a crash or a previous release.
    foreach ($process in $matchingProcesses) {
        Stop-Process -Id $process.ProcessId -Force -ErrorAction Stop
    }
    foreach ($process in $matchingProcesses) {
        try { Wait-Process -Id $process.ProcessId -Timeout 10 -ErrorAction Stop } catch {}
    }

    Remove-Item -LiteralPath $pidFile -Force -ErrorAction SilentlyContinue
    Show-StopMessage 'MVSS has been stopped. You can open it again whenever you need.'
} catch {
    Show-StopMessage ('Could not stop MVSS safely.' + "`r`n" + $_.Exception.Message) $true
    exit 1
}
