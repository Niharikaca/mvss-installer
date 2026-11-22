# MVSS Windows Installer

MVSS (MV Statistical Solution) is a Windows application for statistical analysis.

## Version 1.0.1 (startup reliability update)

This installer includes Portable R 4.6.2 and the packages MVSS needs. It is for 64-bit Windows and installs to `C:\Program Files\MVSS` by default.

## Install and use

1. Download `MVSS-Setup-1.0.1.exe` from the [MVSS Releases page](https://github.com/Niharikaca/mvss-installer/releases).
2. Run the installer and follow the steps.
3. Open **MVSS** from the Start Menu or desktop shortcut.
4. MVSS opens in your default browser at `http://127.0.0.1:4242`. It runs locally on your computer.

No separate R or RStudio installation is needed. Internet is not required after installation.

## Close MVSS

Closing the browser tab does not always stop the local app. To stop it safely, open the Start Menu and choose **Stop MVSS**. You can launch MVSS again whenever you need it.

## Troubleshooting

- If MVSS is already open, launching it again should open the existing session.
- If another application uses port 4242, MVSS will explain the problem rather than stopping that application.
- If startup fails, a message will tell you where to find the diagnostic log under `%LOCALAPPDATA%\MVSS\logs`.
- To update, install the newer release over the existing installation. Your files stored outside the installation folder are not removed by the installer.

## Uninstall

Use **Settings > Apps > Installed apps > MVSS > Uninstall**, or choose **Uninstall MVSS** from the Start Menu.

## Build from source

Run `build_installer.R` on Windows with Portable R and Inno Setup 6 available. The installer is written to the `installer` folder.
