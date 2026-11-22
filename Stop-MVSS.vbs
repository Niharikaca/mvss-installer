Set shell = CreateObject("WScript.Shell")
Set fso = CreateObject("Scripting.FileSystemObject")

installFolder = fso.GetParentFolderName(WScript.ScriptFullName)
stopScript = installFolder & "\Stop-MVSS.ps1"

If Not fso.FileExists(stopScript) Then
    MsgBox "The MVSS stop tool is missing. Please reinstall MVSS.", _
        vbCritical, "MVSS"
    WScript.Quit 1
End If

powershell = "powershell.exe -NoProfile -ExecutionPolicy Bypass -File """ & stopScript & """"
shell.Run powershell, 0, False
