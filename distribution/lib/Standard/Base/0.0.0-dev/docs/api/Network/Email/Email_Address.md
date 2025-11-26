## Enso Signatures 1.0
## module Standard.Base.Network.Email.Email_Address
- type Email_Address
    - Address address:Standard.Base.Data.Text.Text= name:Standard.Base.Data.Text.Text=
    - default_widget display:Standard.Base.Metadata.Display= -> Standard.Base.Metadata.Widget
    - to_text self -> Standard.Base.Data.Text.Text
- Standard.Base.Network.Email.Email_Address.Email_Address.from that:Standard.Base.Data.Text.Text -> Standard.Base.Network.Email.Email_Address.Email_Address
