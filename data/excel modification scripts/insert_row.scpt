-- Arguments: filePath, rowNumber
on run argv
    set filePath to POSIX file (item 1 of argv) as string
    set rowNum to item 2 of argv as integer
    
    tell application "Microsoft Excel"
        activate
        open workbook workbook file name filePath
        
        set ws to active sheet of active workbook
        set insertRange to range (rowNum & ":" & rowNum) of ws
        insertRange insert shift down
        
        save active workbook
        close active workbook saving yes
    end tell
end run