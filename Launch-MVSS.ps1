$ErrorActionPreference = 'Stop'

function Test-LocalPort([int]$Port) {
    $client = [System.Net.Sockets.TcpClient]::new()
    try {
        $connect = $client.BeginConnect('127.0.0.1', $Port, $null, $null)
        if (-not $connect.AsyncWaitHandle.WaitOne(750)) { return $false }
        $client.EndConnect($connect)
        return $true
    } catch {
        return $false
    } finally {
        $client.Dispose()
    }
}

function Get-LocalPage([string]$Url) {
    try {
        $response = Invoke-WebRequest -UseBasicParsing -Uri $Url -TimeoutSec 3
        return [pscustomobject]@{
            StatusCode = [int]$response.StatusCode
            Content = [string]$response.Content
        }
    } catch {
        return $null
    }
}

function Test-MvssPage($Page) {
    return ($null -ne $Page -and
        $Page.StatusCode -eq 200 -and
        $Page.Content.Contains('MV Statistical Solution'))
}

$serverProcess = $null
$startupMutex = $null
$mutexOwned = $false
try {
    $portText = if ($env:MVSS_PORT) { $env:MVSS_PORT } else { '4242' }
    $port = 0
    if (-not [int]::TryParse($portText, [ref]$port) -or $port -lt 1 -or $port -gt 65535) {
        throw 'MVSS_PORT must be a valid TCP port from 1 to 65535.'
    }
    $url = "http://127.0.0.1:$port/"

    # Keep launches in this Windows session serialized. This prevents two rapid
    # clicks from racing to start separate R/Shiny processes on the same port.
    $startupMutex = [System.Threading.Mutex]::new($false, 'Local\MVSS.Startup')
    $mutexDeadline = (Get-Date).AddSeconds(70)
    while (-not $mutexOwned -and (Get-Date) -lt $mutexDeadline) {
        try {
            $mutexOwned = $startupMutex.WaitOne(0)
        } catch [System.Threading.AbandonedMutexException] {
            # The previous launcher crashed; this process now owns the mutex.
            $mutexOwned = $true
        }

        if ($mutexOwned) { break }

        # While another launcher owns the lock, reuse its app as soon as it is
        # ready. If it fails, the mutex becomes available and this launch starts.
        if (Test-MvssPage (Get-LocalPage $url)) {
            Start-Process $url
            exit 0
        }
        Start-Sleep -Milliseconds 350
    }
    if (-not $mutexOwned) {
        throw 'MVSS is still starting in another window. Please wait a moment, then open MVSS again.'
    }

    if (Test-LocalPort $port) {
        # A server may be bound before Shiny finishes serving its page.
        $existingDeadline = (Get-Date).AddSeconds(15)
        while ((Get-Date) -lt $existingDeadline) {
            if (Test-MvssPage (Get-LocalPage $url)) {
                Start-Process $url
                exit 0
            }
            Start-Sleep -Milliseconds 500
        }

        throw "Port $port is already in use, but it did not respond as MVSS. Close the application using that port or set MVSS_PORT to another unused port."
    }

    $root = $PSScriptRoot
    $runtime = Join-Path $root 'R-portable\bin\Rscript.exe'
    $runner = Join-Path $root 'Launch-MVSS.R'
    $app = Join-Path $root 'app'
    foreach ($required in @($runtime, $runner, $app)) {
        if (-not (Test-Path -LiteralPath $required)) {
            throw "Required bundled MVSS file is missing: $required"
        }
    }

    $versionOutput = (& $runtime --version 2>&1 | Out-String).Trim()
    if ($versionOutput -notmatch '\(R\) version 4\.6\.2') {
        throw "The bundled runtime must be R 4.6.2. Found: $versionOutput"
    }

    $env:R_HOME = Join-Path $root 'R-portable'
    $env:R_LIBS_USER = ''
    $env:R_LIBS_SITE = ''
    $env:R_DEFAULT_PACKAGES = 'NULL'
    $env:MVSS_PORT = [string]$port

    $logDir = Join-Path (Join-Path $env:LOCALAPPDATA 'MVSS') 'logs'
    New-Item -ItemType Directory -Path $logDir -Force | Out-Null
    $stdout = Join-Path $logDir 'server-output.log'
    $stderr = Join-Path $logDir 'server-error.log'
    $serverProcess = Start-Process -FilePath $runtime `
        -ArgumentList @('--vanilla', ('"{0}"' -f $runner), ('"{0}"' -f $app)) `
        -WorkingDirectory $root -WindowStyle Hidden -PassThru `
        -RedirectStandardOutput $stdout -RedirectStandardError $stderr
    $pidFile = Join-Path $dataDir 'server.pid'
    Set-Content -LiteralPath $pidFile -Value $serverProcess.Id -Encoding Ascii

    $ready = $false
    $deadline = (Get-Date).AddSeconds(60)
    while ((Get-Date) -lt $deadline) {
        if ($serverProcess.HasExited) {
            $details = if (Test-Path $stderr) { Get-Content -LiteralPath $stderr -Raw } else { '' }
            throw "MVSS server stopped before it became ready. $details"
        }
        if (Test-MvssPage (Get-LocalPage $url)) {
            $ready = $true
            break
        }
        Start-Sleep -Milliseconds 500
    }
    if (-not $ready) { throw "MVSS did not become ready within 60 seconds. Please try again or check the diagnostic log." }

    Start-Process $url

    # Keep the mutex while the server is alive so later launches reuse it.
    while (-not $serverProcess.HasExited) { Start-Sleep -Seconds 2 }
} catch {
    $message = $_.Exception.Message
    $logPath = Join-Path (Join-Path (Join-Path $env:LOCALAPPDATA 'MVSS') 'logs') 'launcher-error.log'
    try {
        New-Item -ItemType Directory -Path (Split-Path $logPath) -Force | Out-Null
        Add-Content -LiteralPath $logPath -Value ("[{0}] {1}`r`n{2}" -f (Get-Date), $message, ($_ | Out-String))
    } catch {}
    try {
        Add-Type -AssemblyName System.Windows.Forms
        [System.Windows.Forms.MessageBox]::Show(
            ($message + "`r`n`r`nDetails saved to: " + $logPath),
            'MVSS Startup Error',
            [System.Windows.Forms.MessageBoxButtons]::OK,
            [System.Windows.Forms.MessageBoxIcon]::Error
        ) | Out-Null
    } catch {}
    exit 1
} finally {
    if ($null -ne $serverProcess) {
        if (-not $serverProcess.HasExited) {
            Stop-Process -Id $serverProcess.Id -Force -ErrorAction SilentlyContinue
        }
        if ($null -ne $pidFile -and (Test-Path -LiteralPath $pidFile)) {
            try {
                if ((Get-Content -LiteralPath $pidFile -Raw).Trim() -eq [string]$serverProcess.Id) {
                    Remove-Item -LiteralPath $pidFile -Force -ErrorAction SilentlyContinue
                }
            } catch {}
        }
    }
    if ($mutexOwned -and $null -ne $startupMutex) {
        try { $startupMutex.ReleaseMutex() } catch {}
    }
    if ($null -ne $startupMutex) { $startupMutex.Dispose() }
}
