# Load this file using ". Init-Git-Unix-Environment.ps1"
#
# TODO(oli.legat) - fix hard-coded path  
$Env:PATH += "C:\Users\olegat\AppData\Local\Programs\Git\usr\bin;"

# Use ls.exe instead of Get-ChildItem 
function Git-ls-autocolor {
  C:\Users\olegat\AppData\Local\Programs\Git\usr\bin\ls.exe --color=auto $args
}
Set-Alias -Name ls -Value Git-ls-autocolor -Option AllScope
