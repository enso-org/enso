## Enso Signatures 1.0
## module Standard.Base.Network.Email.Email_Address
- type Email_Address
    - Address address:Standard.Base.Data.Text.Text=
    - Name_Address name:Standard.Base.Data.Text.Text= address:Standard.Base.Data.Text.Text=
    - default_widget display:Standard.Base.Metadata.Display= -> Standard.Base.Metadata.Widget
    - to_text self -> Standard.Base.Data.Text.Text
    - vector_widget display:Standard.Base.Metadata.Display= -> Standard.Base.Metadata.Widget
