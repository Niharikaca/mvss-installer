@echo off
setlocal
set "ROOT=%~dp0"
set "R_HOME=%ROOT%R-portable"
set "R_LIBS_USER="
set "R_LIBS_SITE="
set "R_DEFAULT_PACKAGES=NULL"
if not exist "%ROOT%R-portable\bin\Rscript.exe" (
  echo MVSS portable R was not found. Please reinstall MVSS.
  pause
  exit /b 1
)
"%ROOT%R-portable\bin\Rscript.exe" "%ROOT%Launch-MVSS.R" "%ROOT%app"
if errorlevel 1 (
  echo MVSS could not start. See the error above.
  pause
)
endlocal
