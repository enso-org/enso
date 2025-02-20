## Enso Signatures 1.0
## module Standard.Base.Network.HTTP.HTTP_Error
- type HTTP_Error
    - IO_Error uri:Standard.Base.Network.URI.URI message:Standard.Base.Data.Text.Text
    - Status_Error status_code:Standard.Base.Network.HTTP.HTTP_Status_Code.HTTP_Status_Code message:(Standard.Base.Data.Text.Text|Standard.Base.Nothing.Nothing) uri:Standard.Base.Network.URI.URI
    - handle_java_exceptions uri:Standard.Base.Network.URI.URI ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - to_display_text self -> Standard.Base.Any.Any
