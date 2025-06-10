## Enso Signatures 1.0
## module Standard.Base.System.File.Generic.File_Write_Strategy
- type Dry_Run_File_Settings
    - Value existing_file_behavior:Standard.Base.System.File.Existing_File_Behavior.Existing_File_Behavior copy_original:Standard.Base.Data.Boolean.Boolean
- type File_Write_Strategy
    - Value write_overwriting:Standard.Base.Any.Any write_appending:Standard.Base.Any.Any write_raising_error:Standard.Base.Any.Any write_backing_up:Standard.Base.Any.Any create_dry_run_file:Standard.Base.Any.Any write_with_local_file:Standard.Base.Any.Any copy_from_local:Standard.Base.Any.Any
    - write self file:Standard.Base.Any.Any existing_file_behavior:Standard.Base.System.File.Existing_File_Behavior.Existing_File_Behavior action:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - write_handling_dry_run self file:Standard.Base.Any.Any existing_file_behavior:Standard.Base.System.File.Existing_File_Behavior.Existing_File_Behavior action:Standard.Base.Any.Any -> Standard.Base.Any.Any
- default_append file:Standard.Base.Any.Any action:Standard.Base.Any.Any -> Standard.Base.Any.Any
- default_overwrite file:Standard.Base.Any.Any action:Standard.Base.Any.Any -> Standard.Base.Any.Any
- default_raise_error file:Standard.Base.Any.Any action:Standard.Base.Any.Any -> Standard.Base.Any.Any
- dry_run_behavior file:Standard.Base.Any.Any behavior:Standard.Base.System.File.Existing_File_Behavior.Existing_File_Behavior -> Standard.Base.System.File.Generic.File_Write_Strategy.Dry_Run_File_Settings
- generic_copy source:Standard.Base.Any.Any destination:Standard.Base.System.File.Generic.Writable_File.Writable_File replace_existing:Standard.Base.Any.Any -> Standard.Base.Any.Any
- generic_remote_write_with_local_file file:Standard.Base.System.File.Generic.Writable_File.Writable_File existing_file_behavior:Standard.Base.Any.Any action:Standard.Base.Any.Any -> Standard.Base.Any.Any
