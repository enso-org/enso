## Enso Signatures 1.0
## module Standard.Base.System.File.File_Permissions
- type File_Permissions
    - Value owner:(Standard.Base.Data.Vector.Vector Standard.Base.System.File.File_Permissions.Permission) group:(Standard.Base.Data.Vector.Vector Standard.Base.System.File.File_Permissions.Permission) others:(Standard.Base.Data.Vector.Vector Standard.Base.System.File.File_Permissions.Permission)
    - from_java_set permissions:Standard.Base.Data.Text.Text -> Standard.Base.Any.Any
    - group_execute self -> Standard.Base.Any.Any
    - group_read self -> Standard.Base.Any.Any
    - group_write self -> Standard.Base.Any.Any
    - others_execute self -> Standard.Base.Any.Any
    - others_read self -> Standard.Base.Any.Any
    - others_write self -> Standard.Base.Any.Any
    - owner_execute self -> Standard.Base.Any.Any
    - owner_read self -> Standard.Base.Any.Any
    - owner_write self -> Standard.Base.Any.Any
    - to_display_text self -> Standard.Base.Any.Any
    - to_java self -> Standard.Base.Any.Any
- type Permission
    - Execute
    - Read
    - Write
