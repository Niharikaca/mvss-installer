@echo off
setlocal
set "ROOT=%~dp0"
if not exist "%ROOT%Launch-MVSS.ps1" (
  echo MVSS launcher files are incomplete. Please reinstall MVSS.
  pause
  exit /b 1
)
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%ROOT%Launch-MVSS.ps1"
if errorlevel 1 (
  echo MVSS could not start. See the message above.
  pause
)
endlocal
