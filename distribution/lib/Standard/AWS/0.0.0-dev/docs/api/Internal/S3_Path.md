## Enso Signatures 1.0
## module Standard.AWS.Internal.S3_Path
- type S3_Path
    - Value bucket:Standard.Base.Data.Text.Text key:Standard.Base.Data.Text.Text
    - bucket_root self -> Standard.AWS.Internal.S3_Path.S3_Path
    - delimiter -> Standard.Base.Data.Text.Text
    - file_name self -> Standard.Base.Data.Text.Text
    - is_descendant_of self other:Standard.AWS.Internal.S3_Path.S3_Path -> Standard.Base.Data.Boolean.Boolean
    - is_directory self -> Standard.Base.Data.Boolean.Boolean
    - is_root self -> Standard.Base.Data.Boolean.Boolean
    - join self subpaths:Standard.Base.Data.Vector.Vector -> Standard.AWS.Internal.S3_Path.S3_Path
    - parent self -> (Standard.AWS.Internal.S3_Path.S3_Path|Standard.Base.Nothing.Nothing)
    - parse uri:Standard.Base.Data.Text.Text -> Standard.AWS.Internal.S3_Path.S3_Path!Standard.Base.Errors.Illegal_Argument.Illegal_Argument
    - resolve self subpath:Standard.Base.Data.Text.Text -> Standard.AWS.Internal.S3_Path.S3_Path
    - to_display_text self -> Standard.Base.Data.Text.Text
    - to_text self -> Standard.Base.Data.Text.Text
    - without_trailing_slash self -> Standard.AWS.Internal.S3_Path.S3_Path
