$ErrorActionPreference = 'Stop'

$installRoot = $PSScriptRoot
$expectedRuntime = [System.IO.Path]::GetFullPath((Join-Path $installRoot 'R-portable\bin\Rscript.exe'))
$expectedRunner = [System.IO.Path]::GetFullPath((Join-Path $installRoot 'Launch-MVSS.R'))
$pidFile = Join-Path (Join-Path $env:LOCALAPPDATA 'MVSS') 'server.pid'

function Show-StopMessage([string]$Text, [bool]$IsError = $false) {
    Add-Type -AssemblyName System.Windows.Forms
    $icon = if ($IsError) { [System.Windows.Forms.MessageBoxIcon]::Warning } else { [System.Windows.Forms.MessageBoxIcon]::Information }
    [System.Windows.Forms.MessageBox]::Show($Text, 'MVSS', [System.Windows.Forms.MessageBoxButtons]::OK, $icon) | Out-Null
}

try {
    if (-not (Test-Path -LiteralPath $pidFile)) {
        Show-StopMessage 'MVSS is not running.'
        exit 0
    }

    $serverPid = 0
    $pidText = (Get-Content -LiteralPath $pidFile -Raw).Trim()
    if (-not [int]::TryParse($pidText, [ref]$serverPid) -or $serverPid -lt 1) {
        Remove-Item -LiteralPath $pidFile -Force -ErrorAction SilentlyContinue
        Show-StopMessage 'The saved MVSS process information was invalid. Please launch MVSS again.' $true
        exit 1
    }

    $process = Get-CimInstance Win32_Process -Filter "ProcessId = $serverPid" -ErrorAction SilentlyContinue
    if ($null -eq $process) {
        Remove-Item -LiteralPath $pidFile -Force -ErrorAction SilentlyContinue
        Show-StopMessage 'MVSS was already closed.'
        exit 0
    }

    $commandLine = [string]$process.CommandLine
    $isMvss = ([string]$process.ExecutablePath -ieq $expectedRuntime) -and
        ($commandLine.IndexOf($expectedRunner, [System.StringComparison]::OrdinalIgnoreCase) -ge 0)
    if (-not $isMvss) {
        Show-StopMessage 'The saved process no longer belongs to MVSS. It was not stopped, to protect other applications.' $true
        exit 1
    }

    Stop-Process -Id $serverPid -Force
    try { Wait-Process -Id $serverPid -Timeout 10 -ErrorAction Stop } catch {}
    Remove-Item -LiteralPath $pidFile -Force -ErrorAction SilentlyContinue
    Show-StopMessage 'MVSS has been stopped. You can open it again whenever you need.'
} catch {
    Show-StopMessage ('Could not stop MVSS safely.' + "`r`n" + $_.Exception.Message) $true
    exit 1
}
