#ifndef SourceRoot
  #error SourceRoot must be supplied by build_installer.R
#endif
#ifndef OutputRoot
  #error OutputRoot must be supplied by build_installer.R
#endif

[Setup]
AppId={{3F611C78-5B2B-42B5-9C39-2D310AD69D05}
AppName=MVSS
AppVersion=1.0.0
AppVerName=MVSS 1.0.0
AppPublisher=MVSS
DefaultDirName={autopf}\MVSS
DefaultGroupName=MVSS
UninstallDisplayName=MVSS 1.0.0
OutputDir={#OutputRoot}
OutputBaseFilename=MVSS-Setup-1.0.0
Compression=lzma
SolidCompression=yes
WizardStyle=modern
ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64
PrivilegesRequired=admin
CloseApplications=yes
Uninstallable=yes

[Files]
Source: "{#SourceRoot}\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs

[Icons]
Name: "{group}\MVSS"; Filename: "{app}\Launch-MVSS.bat"; WorkingDir: "{app}"
Name: "{autodesktop}\MVSS"; Filename: "{app}\Launch-MVSS.bat"; WorkingDir: "{app}"; Tasks: desktopicon

[Tasks]
Name: "desktopicon"; Description: "Create a desktop shortcut"; GroupDescription: "Additional shortcuts:"; Flags: unchecked

[Run]
Filename: "{app}\Launch-MVSS.bat"; Description: "Launch MVSS"; Flags: postinstall nowait skipifsilent
