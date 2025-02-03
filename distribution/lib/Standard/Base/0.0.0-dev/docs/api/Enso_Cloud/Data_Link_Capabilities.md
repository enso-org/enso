## Enso Signatures 1.0
## module Standard.Base.Enso_Cloud.Data_Link_Capabilities
- type Data_Link_With_Input_Stream
    - Value underlying:Standard.Base.Any.Any
    - find data_link_instance:Standard.Base.Any.Any if_not_supported:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - with_input_stream self open_options:Standard.Base.Data.Vector.Vector action:Standard.Base.Any.Any -> Standard.Base.Any.Any
- type Data_Link_With_Output_Stream
    - Value underlying:Standard.Base.Any.Any
    - find data_link_instance:Standard.Base.Any.Any if_not_supported:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - with_output_stream self open_options:Standard.Base.Data.Vector.Vector action:Standard.Base.Any.Any -> Standard.Base.Any.Any
- type File_Like_Data_Link
    - Value underlying:Standard.Base.Any.Any as_file:Standard.Base.Any.Any
    - find data_link_instance:Standard.Base.Any.Any if_not_supported:Standard.Base.Any.Any= -> Standard.Base.Any.Any
- type Writable_Data_Link
    - Value underlying:Standard.Base.Any.Any
    - as_writable_file self -> Standard.Base.System.File.Generic.Writable_File.Writable_File
    - find data_link_instance:Standard.Base.Any.Any if_not_supported:Standard.Base.Any.Any= -> Standard.Base.Any.Any
