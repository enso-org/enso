## Enso Signatures 1.0
## module Standard.Base.Enso_Cloud.Data_Link
- type Data_Link
    - copy source:Standard.Base.System.File.Generic.File_Like.File_Like target:Standard.Base.System.File.Generic.File_Like.File_Like replace_existing:Standard.Base.Data.Boolean.Boolean= -> Standard.Base.Any.Any
    - is_data_link_file file:Standard.Base.Any.Any -> Standard.Base.Data.Boolean.Boolean
    - is_data_link_from_metadata file_metadata:Standard.Base.System.File_Format_Metadata.File_Format_Metadata -> Standard.Base.Data.Boolean.Boolean
    - is_data_link_name name:Standard.Base.Data.Text.Text -> Standard.Base.Data.Boolean.Boolean
    - move source:Standard.Base.System.File.Generic.File_Like.File_Like target:Standard.Base.System.File.Generic.File_Like.File_Like replace_existing:Standard.Base.Data.Boolean.Boolean= -> Standard.Base.Any.Any
    - read_config file:Standard.Base.System.File.Generic.File_Like.File_Like -> Standard.Base.Data.Json.JS_Object
    - read_raw_config file:Standard.Base.System.File.Generic.File_Like.File_Like -> Standard.Base.Data.Text.Text
    - validate_config config:Standard.Base.Data.Json.JS_Object -> Standard.Base.Any.Any
    - write_config file:Standard.Base.System.File.Generic.File_Like.File_Like config:Standard.Base.Data.Json.JS_Object replace_existing:Standard.Base.Data.Boolean.Boolean= skip_validation:Standard.Base.Data.Boolean.Boolean= -> Standard.Base.Any.Any
    - write_raw_config file:Standard.Base.System.File.Generic.File_Like.File_Like raw_content:Standard.Base.Data.Text.Text replace_existing:Standard.Base.Data.Boolean.Boolean= -> Standard.Base.Any.Any
- type Data_Link_From_File
    - Value underlying:Standard.Base.Any.Any
    - is_data_link self -> Standard.Base.Data.Boolean.Boolean
- Standard.Base.Enso_Cloud.Data_Link.Data_Link_From_File.from that:Standard.Base.System.File.Generic.File_Like.File_Like -> Standard.Base.Enso_Cloud.Data_Link.Data_Link_From_File
