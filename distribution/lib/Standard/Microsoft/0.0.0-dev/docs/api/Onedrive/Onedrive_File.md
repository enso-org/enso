## Enso Signatures 1.0
## module Standard.Microsoft.Onedrive.Onedrive_File
- type Onedrive_File
    - Value service:Standard.Microsoft.Microsoft365.Microsoft365 path:Standard.Microsoft.Onedrive.Onedrive_File.Onedrive_Path
    - list self -> (Standard.Base.Data.Vector.Vector Standard.Microsoft.Onedrive.Onedrive_File.Onedrive_File)
    - read self -> Standard.Base.Data.Text.Text
    - root service:Standard.Microsoft.Microsoft365.Microsoft365 -> Standard.Base.Any.Any
- type Onedrive_Path
    - Value path:(Standard.Base.Data.Vector.Vector Standard.Base.Data.Text.Text)
    - extend self subpath:Standard.Base.Data.Text.Text -> Standard.Base.Any.Any
    - to_human_readable_fragment_dir self -> Standard.Base.Any.Any
    - to_human_readable_fragment_file self -> Standard.Base.Any.Any
