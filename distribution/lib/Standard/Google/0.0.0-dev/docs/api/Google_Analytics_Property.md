## Enso Signatures 1.0
## module Standard.Google_Api.Google_Analytics_Property
- type Google_Analytics_Property
    - account self -> Standard.Base.Any.Any
    - create_time self -> Standard.Base.Any.Any
    - currency self -> Standard.Base.Any.Any
    - default_widget self_arg:Standard.Base.Any.Any cache:Standard.Base.Any.Any display:Standard.Base.Metadata.Display= -> Standard.Base.Metadata.Widget
    - dimensions self credentials:Standard.Google_Api.Google_Credential.Google_Credential= -> Standard.Base.Any.Any
    - id self -> Standard.Base.Any.Any
    - metrics self credentials:Standard.Google_Api.Google_Credential.Google_Credential= -> Standard.Base.Any.Any
    - name self -> Standard.Base.Any.Any
    - time_zone self -> Standard.Base.Any.Any
    - to_display_text self -> Standard.Base.Any.Any
    - to_text self -> Standard.Base.Any.Any
- Standard.Google_Api.Google_Analytics_Property.Google_Analytics_Property.from that:Standard.Base.Data.Text.Text -> Standard.Google_Api.Google_Analytics_Property.Google_Analytics_Property
- Standard.Google_Api.Google_Analytics_Property.Google_Analytics_Property.from that:Standard.Base.Data.Vector.Vector -> Standard.Google_Api.Google_Analytics_Property.Google_Analytics_Property
