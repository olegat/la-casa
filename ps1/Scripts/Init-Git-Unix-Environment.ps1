# Load this file using ". Init-Git-Unix-Environment.ps1"
#
$Env:PATH += "${Env:GIT_ROOT}\usr\bin;"

# Use ls.exe instead of Get-ChildItem 
function Git-ls-autocolor {
  &"${Env:GIT_ROOT}\usr\bin\ls.exe" --color=auto $args
}
Set-Alias -Name ls -Value Git-ls-autocolor -Option AllScope
