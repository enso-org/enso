## Enso Signatures 1.0
## module Standard.Database.Internal.Upload.Table_Upload_Implementation
- type Table_Upload_Implementation
    - delete_rows self target_table:Standard.Base.Any.Any key_values_to_delete:Standard.Table.Table.Table key_columns:Standard.Base.Any.Any= allow_duplicate_matches:Standard.Base.Data.Boolean.Boolean= -> Standard.Base.Any.Any
    - select_into_database_table self target_table:Standard.Base.Any.Any connection:Standard.Base.Any.Any table_name:Standard.Base.Data.Text.Text primary_key:Standard.Base.Any.Any= temporary:Standard.Base.Any.Any= on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior= -> Standard.Base.Any.Any
    - update_rows self target_table:Standard.Base.Any.Any source_table:Standard.Table.Table.Table update_action:Standard.Database.Update_Action.Update_Action= key_columns:(Standard.Base.Data.Vector.Vector|Standard.Base.Nothing.Nothing)= error_on_missing_columns:Standard.Base.Data.Boolean.Boolean= on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior= -> Standard.Base.Any.Any
- table_upload_from_implementation value:Standard.Base.Any.Any implementation:Standard.Base.Any.Any -> Standard.Base.Any.Any
