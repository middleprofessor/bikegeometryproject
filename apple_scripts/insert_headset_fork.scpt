on run argv
    set filePath to POSIX file (item 1 of argv) as alias
    set rowNum to (item 2 of argv) as integer

    tell application "Microsoft Excel"
        activate
        open filePath
        
        set ws to active sheet of active workbook

        -- Insert six ENTIRE rows at rowNum (reselect each time; no loops)
        set oneRow to rows rowNum of ws
        insert into range oneRow shift shift down

        set oneRow to rows rowNum of ws
        insert into range oneRow shift shift down

        set oneRow to rows rowNum of ws
        insert into range oneRow shift shift down

        set oneRow to rows rowNum of ws
        insert into range oneRow shift shift down

        set oneRow to rows rowNum of ws
        insert into range oneRow shift shift down

        set oneRow to rows rowNum of ws
        insert into range oneRow shift shift down

        -- Populate column A of the six new rows (no loops)
        set value of cell ("A" & rowNum) of ws to "head_tube_d"
        set value of cell ("A" & (rowNum + 1)) of ws to "headset_upper"
        set value of cell ("A" & (rowNum + 2)) of ws to "headset_lower"
        set value of cell ("A" & (rowNum + 3)) of ws to "fork_steerer"
        set value of cell ("A" & (rowNum + 4)) of ws to "fork_steerer_d"
        set value of cell ("A" & (rowNum + 5)) of ws to "fork_material"

        save active workbook
        close active workbook saving yes
    end tell
end run
