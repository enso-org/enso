## Enso Signatures 1.0
## module Standard.Base.Enso_Cloud.Errors
- type Cloud_Session_Expired
    - Error underlying_error:Standard.Base.Any.Any
    - to_display_text self -> Standard.Base.Any.Any
- type Enso_Cloud_Error
    - Connection_Error cause:Standard.Base.Any.Any
    - Invalid_Response_Payload cause:(Standard.Base.Data.Text.Text|Standard.Base.Any.Any)
    - Unauthorized
    - Unexpected_Service_Error method:Standard.Base.Network.HTTP.HTTP_Method.HTTP_Method uri:Standard.Base.Network.URI.URI status_code:Standard.Base.Network.HTTP.HTTP_Status_Code.HTTP_Status_Code payload:Standard.Base.Data.Text.Text
    - to_display_text self -> Standard.Base.Any.Any
- type Missing_Data_Link_Library
    - Error library_name:Standard.Base.Data.Text.Text data_link_type:Standard.Base.Data.Text.Text
    - to_display_text self -> Standard.Base.Any.Any
- type Not_Logged_In
    - Error
    - to_display_text self -> Standard.Base.Any.Any
