## Enso Signatures 1.0
## module Standard.Microsoft.OneDrive
- type OneDrive
    - new path:Standard.Base.Data.Text.Text= credential:(Standard.Base.System.File.File|Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret)= -> Standard.Microsoft.OneDrive.OneDrive_File
    - root credential:(Standard.Base.System.File.File|Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret)= -> Standard.Microsoft.OneDrive.OneDrive_File
- type OneDrive_File
    - is_directory self -> Standard.Base.Data.Boolean.Boolean
    - list self name_filter:Standard.Base.Data.Text.Text= recursive:Standard.Base.Data.Boolean.Boolean= -> (Standard.Base.Data.Vector.Vector Standard.Microsoft.OneDrive.OneDrive_File)
    - name self -> Standard.Base.Data.Text.Text
    - path self -> Standard.Base.Data.Text.Text
    - read self format:Standard.Base.Any.Any= on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior= -> Standard.Base.Any.Any!Standard.Base.Errors.Illegal_Argument.Illegal_Argument
    - root credential:Standard.Microsoft.Microsoft365_Credential.Microsoft365_Credential -> Standard.Base.Any.Any
    - to_display_text self -> Standard.Base.Any.Any
    - to_text self -> Standard.Base.Any.Any
    - uri self -> Standard.Base.Any.Any
    - with_input_stream self open_options:Standard.Base.Data.Vector.Vector action:Standard.Base.Any.Any -> Standard.Base.Any.Any
