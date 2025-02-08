## Enso Signatures 1.0
## module Standard.Base.System.File.Advanced.Temporary_File
- type Temporary_File
    - Instance file_resource_reference:(Standard.Base.Runtime.Ref.Ref Standard.Base.Any.Any)
    - access_resource self -> Standard.Base.Any.Any
    - dispose self -> Standard.Base.Any.Any
    - from_stream stream:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - from_stream_light stream:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - new prefix:Standard.Base.Any.Any= suffix:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - to_text self -> Standard.Base.Any.Any
    - unsafe_get self -> Standard.Base.Any.Any
    - with_file self action:Standard.Base.Any.Any -> Standard.Base.Any.Any
- cleanup_tmp_file file:Standard.Base.Any.Any -> Standard.Base.Any.Any
