## Enso Signatures 1.0
## module Standard.Table.Excel.Excel_Format
- type Excel_Format
    - Range address:(Standard.Base.Data.Text.Text|Standard.Table.Excel.Excel_Range.Excel_Range)= headers:Standard.Table.Headers.Headers= skip_rows:Standard.Base.Data.Numbers.Integer= row_limit:Standard.Table.Rows_To_Read.Rows_To_Read= xls_format:(Standard.Table.Excel.Excel_File_Format.Excel_File_Format|Standard.Base.System.File_Format.Infer)=
    - Sheet sheet:(Standard.Base.Data.Numbers.Integer|Standard.Base.Data.Text.Text)= headers:Standard.Table.Headers.Headers= skip_rows:Standard.Base.Data.Numbers.Integer= row_limit:Standard.Table.Rows_To_Read.Rows_To_Read= xls_format:(Standard.Table.Excel.Excel_File_Format.Excel_File_Format|Standard.Base.System.File_Format.Infer)=
    - Workbook xls_format:(Standard.Table.Excel.Excel_File_Format.Excel_File_Format|Standard.Base.System.File_Format.Infer)= default_sheet:Standard.Base.Data.Text.Text=
    - for_file_write file:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - for_read file:Standard.Base.System.File_Format_Metadata.File_Format_Metadata -> Standard.Base.Any.Any
    - get_dropdown_options -> Standard.Base.Any.Any
    - get_name_patterns -> (Standard.Base.Data.Vector.Vector Standard.Base.System.File_Format.File_Name_Pattern)
    - read self file:Standard.Base.Any.Any on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Base.Any.Any
    - read_stream self stream:Standard.Base.System.Input_Stream.Input_Stream metadata:Standard.Base.System.File_Format_Metadata.File_Format_Metadata= -> Standard.Base.Any.Any
    - resolve constructor:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - write_table self file:Standard.Base.Any.Any table:Standard.Base.Any.Any on_existing_file:Standard.Base.Any.Any match_columns:Standard.Base.Any.Any on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Base.Any.Any
- as_section format:Standard.Table.Excel.Excel_Format.Excel_Format -> Standard.Table.Internal.Excel_Section.Excel_Section
- resolve_xl_format xl_format:Standard.Base.Any.Any file:Standard.Base.System.File_Format_Metadata.File_Format_Metadata -> Standard.Base.Any.Any
- xl_format_from_metadata metadata:Standard.Base.System.File_Format_Metadata.File_Format_Metadata -> Standard.Base.Any.Any
- Standard.Table.Excel.Excel_Format.Excel_Format.from that:Standard.Base.Data.Json.JS_Object -> Standard.Table.Excel.Excel_Format.Excel_Format
