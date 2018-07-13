:: this is for batch renaming multiple files

Setlocal enabledelayedexpansion
Set "Old=OLDNAME" :: change the OLDNAME to what you want to replace
Set "New=NEWNAME" :: change the NEWNAMEto what you want the replacement to be
Set "Old_=_R"
Set "New_=-CM"
for /f "tokens=*" %%a in ('dir /b /od *DOTFILENAME ') do ( :: change the DOTFILENAME (.filename) to the filename extension you want to change
    set File=%%~nxa
    set File=!File:%Old_%=%New_%!
    set File=!File:%Old%=%New%!
    ren "%%a" "!File!"
)
Pause&Exit