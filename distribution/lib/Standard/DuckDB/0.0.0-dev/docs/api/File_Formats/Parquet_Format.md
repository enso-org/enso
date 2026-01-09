## Enso Signatures 1.0
## module Standard.DuckDB.File_Formats.Parquet_Format
- type Parquet_Format
    - Parquet compression:Standard.DuckDB.DuckDB_Format.DuckDB_Parquet_Compression= field_ids:Standard.Base.Data.Text.Text= version:Standard.DuckDB.DuckDB_Format.DuckDB_Parquet_Version=
    - for_file_write file:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - for_read file:Standard.Base.System.File_Format_Metadata.File_Format_Metadata -> Standard.Base.Any.Any
    - get_dropdown_options -> Standard.Base.Any.Any
    - get_name_patterns -> (Standard.Base.Data.Vector.Vector Standard.Base.System.File_Format.File_Name_Pattern)
    - read self file:Standard.Base.Any.Any on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Base.Any.Any
    - read_stream self stream:Standard.Base.System.Input_Stream.Input_Stream metadata:Standard.Base.System.File_Format_Metadata.File_Format_Metadata= -> Standard.Base.Any.Any
    - resolve constructor:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - write_table self file:Standard.Base.Any.Any table:Standard.Base.Any.Any on_existing_file:Standard.Base.Any.Any match_columns:Standard.Base.Any.Any on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Base.Any.Any
