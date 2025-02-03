## Enso Signatures 1.0
## module Standard.Base.Network.HTTP.Response_Body
- type Response_Body
    - Materialized_Stream restartable_stream:Standard.Base.System.Advanced.Restartable_Input_Stream.Restartable_Input_Stream metadata:Standard.Base.System.File_Format_Metadata.File_Format_Metadata uri:Standard.Base.Network.URI.URI
    - Raw_Stream raw_stream:Standard.Base.System.Input_Stream.Input_Stream metadata:Standard.Base.System.File_Format_Metadata.File_Format_Metadata uri:Standard.Base.Network.URI.URI
    - content_type self -> Standard.Base.Any.Any
    - decode self format:Standard.Base.Any.Any= ~if_unsupported:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - decode_as_bytes self -> Standard.Base.Any.Any
    - decode_as_json self encoding:(Standard.Base.Data.Text.Encoding.Encoding|Standard.Base.System.File_Format.Infer)= -> Standard.Base.Any.Any
    - decode_as_text self encoding:(Standard.Base.Data.Text.Encoding.Encoding|Standard.Base.System.File_Format.Infer)= -> Standard.Base.Any.Any
    - materialize self -> Standard.Base.Any.Any
    - new stream:Standard.Base.Any.Any metadata:Standard.Base.System.File_Format_Metadata.File_Format_Metadata uri:Standard.Base.Network.URI.URI -> Standard.Base.Any.Any
    - to_text self -> Standard.Base.Any.Any
    - with_stream self action:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - write self file:Standard.Base.System.File.Generic.Writable_File.Writable_File on_existing_file:Standard.Base.Any.Any= -> Standard.Base.Any.Any
- can_decode type:Standard.Base.Any.Any -> Standard.Base.Any.Any
- decode_format_selector -> Standard.Base.Any.Any
- delete_file file:Standard.Base.Any.Any -> Standard.Base.Any.Any
- maximum_body_in_memory -> Standard.Base.Any.Any
