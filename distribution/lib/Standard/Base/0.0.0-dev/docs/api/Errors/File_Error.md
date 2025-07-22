## Enso Signatures 1.0
## module Standard.Base.Errors.File_Error
- type File_Error
    - Access_Denied file:Standard.Base.System.File.Generic.File_Like.File_Like
    - Already_Exists file:Standard.Base.System.File.Generic.File_Like.File_Like
    - Corrupted_Format file:(Standard.Base.System.File.Generic.File_Like.File_Like|Standard.Base.Nothing.Nothing) message:Standard.Base.Data.Text.Text cause:(Standard.Base.Any.Any|Standard.Base.Nothing.Nothing)=
    - Directory_Not_Empty file:Standard.Base.System.File.Generic.File_Like.File_Like
    - IO_Error file:(Standard.Base.System.File.Generic.File_Like.File_Like|Standard.Base.Nothing.Nothing) message:Standard.Base.Data.Text.Text
    - Not_A_Directory file:Standard.Base.System.File.Generic.File_Like.File_Like
    - Not_Found file:Standard.Base.System.File.Generic.File_Like.File_Like
    - Unsupported_Output_Type format:(Standard.Base.System.File_Format.File_Format|Standard.Base.System.File.Generic.File_Like.File_Like|Standard.Base.Any.Any) data_type:Standard.Base.Any.Any
    - Unsupported_Type file:Standard.Base.System.File_Format_Metadata.File_Format_Metadata
    - access_denied file:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - handle_java_exceptions file:(Standard.Base.System.File.File|Standard.Base.Nothing.Nothing) ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - to_display_text self -> Standard.Base.Any.Any
    - wrap_io_exception file:(Standard.Base.System.File.File|Standard.Base.Nothing.Nothing) io_exception:Standard.Base.Any.Any -> Standard.Base.Any.Any
