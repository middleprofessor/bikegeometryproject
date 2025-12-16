-- Usage: osascript insert_row_with_values.scpt "/path/to/file.xlsx" 15
on run argv
    set filePath to POSIX file (item 1 of argv) as alias
    set rowNum to item 2 of argv as integer

    tell application "Microsoft Excel"
        activate
        open filePath
        
        set activeSheet to active sheet of active workbook

        set rowStr to rowNum as string
        set rowp1Str to (rowNum + 1) as string
        set rowp2Str to (rowNum + 2) as string
        set targetRange to ("A" & rowStr & ":" & "B" & rowp2Str) as string
        set insertRange to range targetRange of activeSheet
        insert into range insertRange shift shift down

        set value of cell ("A" & rowStr) of activeSheet to "class"
        set value of cell ("B" & rowStr) of activeSheet to "gravel"
        set value of cell ("A" & rowp1Str) of activeSheet to "handlebar"
        set value of cell ("B" & rowp1Str) of activeSheet to "drop bar"
        set value of cell ("A" & rowp2Str) of activeSheet to "fork"
        set value of cell ("B" & rowp2Str) of activeSheet to "rigid"

        save active workbook
        close active workbook saving yes
    end tell
end run
