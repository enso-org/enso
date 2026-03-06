## Enso Signatures 1.0
## module Standard.Base.System.File.Generic.File_Like
- type File_Like
    - Value underlying:Standard.Base.Any.Any
    - can_write_into_parent self -> Standard.Base.Any.Any
    - copy_to self destination:Standard.Base.Any.Any replace_existing:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - exists self -> Standard.Base.Any.Any
    - extension self -> Standard.Base.Data.Text.Text
    - name self -> Standard.Base.Data.Text.Text
    - parent self -> Standard.Base.System.File.Generic.File_Like.File_Like
    - path self -> Standard.Base.Data.Text.Text
    - to_display_text self -> Standard.Base.Data.Text.Text
- Standard.Base.System.File.Generic.File_Like.File_Like.from that:Standard.Base.Data.Text.Text -> Standard.Base.System.File.Generic.File_Like.File_Like
- Standard.Base.System.File_Format_Metadata.File_Format_Metadata.from that:Standard.Base.System.File.Generic.File_Like.File_Like -> Standard.Base.System.File_Format_Metadata.File_Format_Metadata
