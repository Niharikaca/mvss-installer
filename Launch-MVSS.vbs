Set shell = CreateObject("WScript.Shell")
Set fso = CreateObject("Scripting.FileSystemObject")

installFolder = fso.GetParentFolderName(WScript.ScriptFullName)
launcher = installFolder & "\\Launch-MVSS.ps1"

If Not fso.FileExists(launcher) Then
    MsgBox "The MVSS launcher is missing. Please reinstall MVSS.", _
        vbCritical, "MVSS Startup Error"
    WScript.Quit 1
End If

powershell = "powershell.exe -NoProfile -ExecutionPolicy Bypass -File """ & launcher & """"
' Window style 0 hides the console; False lets this script exit immediately.
shell.Run powershell, 0, False
