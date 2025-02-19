## Enso Signatures 1.0
## module Standard.Base.System.File.Local_File_Write_Strategy
- type Internal_Write_Operation_Errored
    - Error cause:Standard.Base.Any.Any
- type Internal_Write_Operation_Panicked
    - Panic cause:Standard.Base.Panic.Caught_Panic
- catch_already_exists handler:Standard.Base.Any.Any -> Standard.Base.Any.Any
- copy_local_from_local source:Standard.Base.System.File.File destination:Standard.Base.System.File.File -> Standard.Base.Any.Any
- create_dry_run_file file:Standard.Base.Any.Any copy_original:Standard.Base.Any.Any -> Standard.Base.Any.Any
- instance -> Standard.Base.Any.Any
- moving_backup file:Standard.Base.Any.Any action:Standard.Base.Any.Any -> Standard.Base.Any.Any
- recover_io_and_not_found ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
- write_file_backing_up_old_one file:Standard.Base.Any.Any action:Standard.Base.Any.Any -> Standard.Base.Any.Any
- write_with_local_file file:Standard.Base.Any.Any existing_file_behavior:Standard.Base.Any.Any action:Standard.Base.Any.Any -> Standard.Base.Any.Any
