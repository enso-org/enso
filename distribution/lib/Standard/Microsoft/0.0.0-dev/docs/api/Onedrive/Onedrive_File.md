## Enso Signatures 1.0
## module Standard.Microsoft.Onedrive.Onedrive_File
- type Onedrive_File
    - Value service:Standard.Microsoft.Microsoft365.Microsoft365 onedrive_path:Standard.Microsoft.Onedrive.Onedrive_File.Onedrive_Path
    - is_directory self -> Standard.Base.Data.Boolean.Boolean
    - list self name_filter:Standard.Base.Data.Text.Text= recursive:Standard.Base.Data.Boolean.Boolean= -> (Standard.Base.Data.Vector.Vector Standard.Microsoft.Onedrive.Onedrive_File.Onedrive_File)
    - name self -> Standard.Base.Data.Text.Text
    - path self -> Standard.Base.Data.Text.Text
    - read self format:Standard.Base.Any.Any= on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior= -> Standard.Base.Any.Any!Standard.Base.Errors.Illegal_Argument.Illegal_Argument
    - root service:Standard.Microsoft.Microsoft365.Microsoft365 -> Standard.Base.Any.Any
    - with_input_stream self open_options:Standard.Base.Data.Vector.Vector action:Standard.Base.Any.Any -> Standard.Base.Any.Any
- type Onedrive_Path
    - Value path:(Standard.Base.Data.Vector.Vector Standard.Base.Data.Text.Text)
    - extend self subpath:Standard.Base.Data.Text.Text -> Standard.Base.Any.Any
    - file_name self -> Standard.Base.Data.Text.Text
    - to_human_readable_fragment self -> Standard.Base.Any.Any
    - to_human_readable_fragment_dir self -> Standard.Base.Any.Any
    - to_human_readable_fragment_file self -> Standard.Base.Any.Any
    - to_text self -> Standard.Base.Data.Text.Text
