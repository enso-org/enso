## Enso Signatures 1.0
## module Standard.Base.System.File_Format_Metadata
- type Content_Type_Metadata
    - Value segments:(Standard.Base.Data.Vector.Vector Standard.Base.Data.Text.Text)
    - base_type self -> Standard.Base.Any.Any
    - encoding self -> Standard.Base.Any.Any
    - find_segment self prefix:Standard.Base.Data.Text.Text -> Standard.Base.Any.Any
    - raw_charset self -> Standard.Base.Any.Any
- type File_Format_Metadata
    - Value path:(Standard.Base.Data.Text.Text|Standard.Base.Nothing.Nothing)= name:(Standard.Base.Data.Text.Text|Standard.Base.Nothing.Nothing)= extension:(Standard.Base.Data.Text.Text|Standard.Base.Nothing.Nothing)= read_first_bytes:Standard.Base.Any.Any= content_type:(Standard.Base.Data.Text.Text|Standard.Base.Nothing.Nothing)=
    - guess_extension self -> Standard.Base.Any.Any
    - interpret_content_type self -> Standard.Base.Any.Any
    - no_information -> Standard.Base.Any.Any
    - to_display_text self -> Standard.Base.Data.Text.Text
- Standard.Base.System.File_Format_Metadata.File_Format_Metadata.from that:Standard.Base.System.File.File -> Standard.Base.System.File_Format_Metadata.File_Format_Metadata
