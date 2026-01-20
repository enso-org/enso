## Enso Signatures 1.0
## module Standard.Table.IO.EDI_Format
- type EDI_Definition
    - Custom structure:Standard.Base.Data.Text.Text field_map:(Standard.Base.Data.Dictionary.Dictionary Standard.Base.Data.Text.Text Standard.Base.Any.Any)=
    - EDI_822
    - From_Json path:Standard.Base.Any.Any
    - Raw_Segments
    - as_custom self -> Standard.Base.Any.Any
- type EDI_Format
    - EDI segment_end:Standard.Base.Data.Text.Text= definition:Standard.Table.IO.EDI_Format.EDI_Definition= map_fields:Standard.Base.Data.Boolean.Boolean=
    - for_file_write file:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - for_read file:Standard.Base.System.File_Format_Metadata.File_Format_Metadata -> Standard.Base.Any.Any
    - get_dropdown_options -> Standard.Base.Any.Any
    - get_name_patterns -> (Standard.Base.Data.Vector.Vector Standard.Base.System.File_Format.File_Name_Pattern)
    - read self file:Standard.Base.Any.Any on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Base.Any.Any
    - read_stream self stream:Standard.Base.System.Input_Stream.Input_Stream metadata:Standard.Base.System.File_Format_Metadata.File_Format_Metadata= -> Standard.Base.Any.Any
    - resolve constructor:Standard.Base.Any.Any -> Standard.Base.Any.Any
