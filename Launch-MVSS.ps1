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
try {
    $portText = if ($env:MVSS_PORT) { $env:MVSS_PORT } else { '4242' }
    $port = 0
    if (-not [int]::TryParse($portText, [ref]$port) -or $port -lt 1 -or $port -gt 65535) {
        throw 'MVSS_PORT must be a valid TCP port from 1 to 65535.'
    }
    $url = "http://127.0.0.1:$port/"
    Write-Host "Checking MVSS on 127.0.0.1:$port ..."

    if (Test-LocalPort $port) {
        $existingPage = Get-LocalPage $url
        if (Test-MvssPage $existingPage) {
            Start-Process $url
            Write-Host 'An MVSS instance is already running. Opened it in your browser.'
            exit 0
        }
        throw "Port $port on 127.0.0.1 is already being used by another application. Close that application or set MVSS_PORT to another unused port, then start MVSS again."
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
    Write-Host 'Using bundled Portable R 4.6.2.'

    $env:R_HOME = Join-Path $root 'R-portable'
    $env:R_LIBS_USER = ''
    $env:R_LIBS_SITE = ''
    $env:R_DEFAULT_PACKAGES = 'NULL'
    $env:MVSS_PORT = [string]$port

    $dataDir = Join-Path $env:LOCALAPPDATA 'MVSS'
    $logDir = Join-Path $dataDir 'logs'
    New-Item -ItemType Directory -Path $logDir -Force | Out-Null
    $stdout = Join-Path $logDir 'server-output.log'
    $stderr = Join-Path $logDir 'server-error.log'
    $serverProcess = Start-Process -FilePath $runtime `
        -ArgumentList @('--vanilla', ('"{0}"' -f $runner), ('"{0}"' -f $app)) `
        -WorkingDirectory $root -WindowStyle Hidden -PassThru `
        -RedirectStandardOutput $stdout -RedirectStandardError $stderr
    Write-Host 'Starting the bundled Shiny application and waiting for localhost readiness...'

    $ready = $false
    $deadline = (Get-Date).AddSeconds(60)
    while ((Get-Date) -lt $deadline) {
        if ($serverProcess.HasExited) {
            $details = if (Test-Path $stderr) { Get-Content -LiteralPath $stderr -Raw } else { '' }
            throw "MVSS server exited before it became ready. $details"
        }
        if (Test-LocalPort $port) {
            $page = Get-LocalPage $url
            if (Test-MvssPage $page) {
                $ready = $true
                break
            }
            if ($null -ne $page) {
                throw "Port $port on 127.0.0.1 began serving a page that is not MVSS. The launcher will not interfere with that service."
            }
        }
        Start-Sleep -Milliseconds 500
    }
    if (-not $ready) { throw "MVSS did not become ready at $url within 60 seconds." }

    Start-Process $url
    Write-Host 'MVSS is ready in your browser.'
    while (-not $serverProcess.HasExited) { Start-Sleep -Seconds 2 }
} catch {
    $message = $_.Exception.Message
    try {
        $logDir = Join-Path (Join-Path $env:LOCALAPPDATA 'MVSS') 'logs'
        New-Item -ItemType Directory -Path $logDir -Force | Out-Null
        $logPath = Join-Path $logDir 'launcher-error.log'
        Add-Content -LiteralPath $logPath -Value ("[{0}] {1}`r`n{2}" -f (Get-Date), $message, ($_ | Out-String))
    } catch {}
    try {
        Add-Type -AssemblyName System.Windows.Forms
        [System.Windows.Forms.MessageBox]::Show(
            ($message + "`r`n`r`nA diagnostic log was saved in %LOCALAPPDATA%\\MVSS\\logs."),
            'MVSS Startup Error',
            [System.Windows.Forms.MessageBoxButtons]::OK,
            [System.Windows.Forms.MessageBoxIcon]::Error
        ) | Out-Null
    } catch {}
    exit 1
} finally {
    if ($null -ne $serverProcess -and -not $serverProcess.HasExited) {
        Stop-Process -Id $serverProcess.Id -Force -ErrorAction SilentlyContinue
    }
}
