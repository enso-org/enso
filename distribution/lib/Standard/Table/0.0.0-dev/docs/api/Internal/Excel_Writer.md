## Enso Signatures 1.0
## module Standard.Table.Internal.Excel_Writer
- find_bak_file base_file:Standard.Base.Any.Any -> Standard.Base.Any.Any
- find_temp_file base_file:Standard.Base.Any.Any -> Standard.Base.Any.Any
- handle_writer ~writer:Standard.Base.Any.Any -> Standard.Base.Any.Any
- make_java_existing_data_mode on_existing_file:Standard.Base.Any.Any match_columns:Standard.Base.Any.Any -> Standard.Base.Any.Any
- make_java_existing_file_behavior on_existing_file:Standard.Base.Any.Any -> Standard.Base.Any.Any
- write_file file:Standard.Base.System.File.Generic.Writable_File.Writable_File table:Standard.Table.Table.Table on_existing_file:Standard.Base.System.File.Existing_File_Behavior.Existing_File_Behavior section:Standard.Table.Internal.Excel_Section.Excel_Section match_columns:Standard.Table.Match_Columns.Match_Columns on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior xls_format:Standard.Table.Excel.Excel_File_Format.Excel_File_Format -> Standard.Base.Any.Any
- write_local_file file:Standard.Base.System.File.File table:Standard.Base.Any.Any on_existing_file:Standard.Base.Any.Any section:Standard.Base.Any.Any match_columns:Standard.Base.Any.Any xls_format:Standard.Base.Any.Any -> Standard.Base.Any.Any
- write_to_workbook java_file:Standard.Base.Any.Any format:Standard.Base.Any.Any table:Standard.Base.Any.Any section:Standard.Base.Any.Any on_existing_file:Standard.Base.Any.Any match_columns:Standard.Base.Any.Any -> Standard.Base.Any.Any
