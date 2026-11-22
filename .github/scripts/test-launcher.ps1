$ErrorActionPreference = 'Stop'
$repoRoot = (Get-Location).Path
$tempRoot = Join-Path $env:RUNNER_TEMP ("mvss-launcher-test-" + [guid]::NewGuid().ToString("N"))
$runtimeBin = Join-Path $tempRoot "R-portable\bin"
$appDir = Join-Path $tempRoot "app"
New-Item -ItemType Directory -Path $runtimeBin,$appDir -Force | Out-Null
Copy-Item (Join-Path $repoRoot "Launch-MVSS.ps1") $tempRoot
Copy-Item (Join-Path $repoRoot "Stop-MVSS.ps1") $tempRoot
New-Item -ItemType File -Path (Join-Path $tempRoot "Launch-MVSS.R") -Force | Out-Null

$project = @'
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net8.0</TargetFramework>
    <AssemblyName>Rscript</AssemblyName>
    <UseAppHost>true</UseAppHost>
  </PropertyGroup>
</Project>
'@
$program = @'
using System;
using System.Net;
using System.Net.Sockets;
using System.Text;
class Program {
  static int Main(string[] args) {
    if (args.Length > 0 && args[0] == "--version") {
      Console.WriteLine("(R) version 4.6.2 (launcher test stub)");
      return 0;
    }
    int port = int.Parse(Environment.GetEnvironmentVariable("MVSS_PORT") ?? "4242");
    var listener = new TcpListener(IPAddress.Loopback, port);
    listener.Start();
    while (true) {
      using var client = listener.AcceptTcpClient();
      using var stream = client.GetStream();
      byte[] request = new byte[4096];
      try { stream.Read(request, 0, request.Length); } catch { }
      byte[] body = Encoding.UTF8.GetBytes("<html><title>MV Statistical Solution</title></html>");
      byte[] header = Encoding.ASCII.GetBytes("HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: " + body.Length + "\r\nConnection: close\r\n\r\n");
      stream.Write(header, 0, header.Length);
      stream.Write(body, 0, body.Length);
    }
  }
}
'@
Set-Content -LiteralPath (Join-Path $tempRoot "FakeRuntime.csproj") -Value $project -Encoding UTF8
Set-Content -LiteralPath (Join-Path $tempRoot "Program.cs") -Value $program -Encoding UTF8
dotnet build (Join-Path $tempRoot "FakeRuntime.csproj") --configuration Release --output $runtimeBin --nologo
if ($LASTEXITCODE -ne 0) { throw "Fake R runtime build failed." }

$probe = [System.Net.Sockets.TcpListener]::new([System.Net.IPAddress]::Loopback, 0)
$probe.Start()
$testPort = ([System.Net.IPEndPoint]$probe.LocalEndpoint).Port
$probe.Stop()
$env:MVSS_PORT = [string]$testPort
$env:MVSS_TEST_MODE = '1'

$launcherPath = Join-Path $tempRoot "Launch-MVSS.ps1"
$powerShell = Join-Path $env:SystemRoot "System32\WindowsPowerShell\v1.0\powershell.exe"
$launchArgs = '-NoProfile -ExecutionPolicy Bypass -File "' + $launcherPath + '"'
$first = Start-Process -FilePath $powerShell -ArgumentList $launchArgs -PassThru -WindowStyle Hidden

try {
  $url = "http://127.0.0.1:$testPort/"
  $ready = $false
  $deadline = (Get-Date).AddSeconds(35)
  while ((Get-Date) -lt $deadline) {
    try {
      $page = Invoke-WebRequest -UseBasicParsing -Uri $url -TimeoutSec 2
      if ($page.Content.Contains("MV Statistical Solution")) { $ready = $true; break }
    } catch {}
    if ($first.HasExited) { throw "First launcher exited before the fake server became ready." }
    Start-Sleep -Milliseconds 250
  }
  if (-not $ready) { throw "Fake MVSS server did not become ready." }

  $second = Start-Process -FilePath $powerShell -ArgumentList $launchArgs -PassThru -WindowStyle Hidden
  if (-not $second.WaitForExit(15000)) { throw "Second launcher did not finish after detecting the running MVSS instance." }
  if ($second.ExitCode -ne 0) { throw "Second launcher returned exit code $($second.ExitCode)." }

  $matching = @(Get-CimInstance Win32_Process -Filter "Name = 'Rscript.exe'" |
    Where-Object { ([string]$_.ExecutablePath -ieq (Join-Path $runtimeBin "Rscript.exe")) -and
      ([string]$_.CommandLine).IndexOf((Join-Path $tempRoot "Launch-MVSS.R"), [System.StringComparison]::OrdinalIgnoreCase) -ge 0 })
  if ($matching.Count -ne 1) { throw "Expected exactly one MVSS server after repeated launch; found $($matching.Count)." }

  $stopPath = Join-Path $tempRoot "Stop-MVSS.ps1"
  $stopArgs = '-NoProfile -ExecutionPolicy Bypass -File "' + $stopPath + '"'
  $stop = Start-Process -FilePath $powerShell -ArgumentList $stopArgs -PassThru -Wait -WindowStyle Hidden
  if ($stop.ExitCode -ne 0) { throw "Stop helper returned exit code $($stop.ExitCode)." }

  if (-not $first.WaitForExit(12000)) { throw "Launcher did not exit after the MVSS server was stopped." }
  $stillOpen = $false
  try {
    Invoke-WebRequest -UseBasicParsing -Uri $url -TimeoutSec 2 | Out-Null
    $stillOpen = $true
  } catch {}
  if ($stillOpen) { throw "MVSS port remained open after Stop MVSS." }
  Write-Host "PASS: hidden launcher starts, repeated launch reuses one server, and Stop MVSS shuts it down."
}
finally {
  if (-not $first.HasExited) { Stop-Process -Id $first.Id -Force -ErrorAction SilentlyContinue }
  Get-CimInstance Win32_Process -Filter "Name = 'Rscript.exe'" -ErrorAction SilentlyContinue |
    Where-Object { ([string]$_.ExecutablePath -ieq (Join-Path $runtimeBin "Rscript.exe")) } |
    ForEach-Object { Stop-Process -Id $_.ProcessId -Force -ErrorAction SilentlyContinue }
  Remove-Item -LiteralPath $tempRoot -Recurse -Force -ErrorAction SilentlyContinue
}
