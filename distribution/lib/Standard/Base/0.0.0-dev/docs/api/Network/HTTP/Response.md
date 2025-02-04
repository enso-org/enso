## Enso Signatures 1.0
## module Standard.Base.Network.HTTP.Response
- type Response
    - Value internal_http_response:Standard.Base.Any.Any body_object:Standard.Base.Any.Any=
    - body self -> Standard.Base.Any.Any
    - code self -> Standard.Base.Any.Any
    - content_length self -> Standard.Base.Any.Any
    - content_type self -> Standard.Base.Any.Any
    - decode self format:Standard.Base.Any.Any= ~if_unsupported:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - decode_as_json self encoding:(Standard.Base.Data.Text.Encoding.Encoding|Standard.Base.System.File_Format.Infer)= -> Standard.Base.Any.Any
    - decode_as_text self encoding:(Standard.Base.Data.Text.Encoding.Encoding|Standard.Base.System.File_Format.Infer)= -> Standard.Base.Any.Any
    - get_header self name:Standard.Base.Data.Text.Text ~if_missing:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - headers self -> Standard.Base.Any.Any
    - to_js_object self -> Standard.Base.Any.Any
    - uri self -> Standard.Base.Any.Any
    - with_materialized_body self -> Standard.Base.Any.Any
    - write self file:Standard.Base.System.File.Generic.Writable_File.Writable_File on_existing_file:Standard.Base.Any.Any= -> Standard.Base.Any.Any
- filename_from_content_disposition content_disposition:Standard.Base.Any.Any -> Standard.Base.Any.Any
- resolve_file_metadata_for_response response:Standard.Base.Any.Any -> Standard.Base.Any.Any
