## Enso Signatures 1.0
## module Standard.Table.YXDB_Format
- type YXDB_Format
    - YXDB_Format
    - for_file_write file:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - for_read file:Standard.Base.System.File_Format_Metadata.File_Format_Metadata -> Standard.Base.Any.Any
    - get_dropdown_options -> Standard.Base.Any.Any
    - get_name_patterns -> (Standard.Base.Data.Vector.Vector Standard.Base.System.File_Format.File_Name_Pattern)
    - read self file:Standard.Base.Any.Any on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Base.Any.Any
    - read_stream self stream:Standard.Base.System.Input_Stream.Input_Stream metadata:Standard.Base.System.File_Format_Metadata.File_Format_Metadata= -> Standard.Base.Any.Any
    - resolve constructor:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - spatial_column_to_geojson table:(Standard.Table.Table.Table&Standard.Table.In_Memory_Table.In_Memory_Table)= column:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)= -> Standard.Base.Any.Any
    - spatial_to_geojson spatial_object:(Standard.Base.Data.Array.Array|Standard.Base.Data.Vector.Vector)= -> Standard.Base.Data.Text.Text
