# This is a Powershell script to remove "-Model" from the filename of PDF prints.
# Run the script by right-clicking and selecting "Run with PowerShell".
# If the script is not working, Powershell may need a permissions adjustment.
# To do this, open Powershell and enter the following:
# set-executionpolicy remotesigned

get-childitem *.pdf | foreach { rename-item $_ $_.Name.Replace("-Model", "") }
get-childitem *.pdf | foreach { rename-item $_ $_.Name.Replace("-Layout1", "") }
get-childitem *.pdf | foreach { rename-item $_ $_.Name.Replace("-Layout", "") }