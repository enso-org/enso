## Enso Signatures 1.0
## module Standard.Base.Network.Email.Email_Provider
- type Email_Provider
    - default_widget display:Standard.Base.Metadata.Display= -> Standard.Base.Metadata.Widget
    - send self email:Standard.Base.Any.Any -> Standard.Base.Any.Any
- type Email_Provider_SPI
    - get_known_email_providers -> Standard.Base.Data.Vector.Vector
    - new typ:Standard.Base.Any.Any -> Standard.Base.Network.Email.Email_Provider.Email_Provider_SPI
- make_email_provider label:Standard.Base.Data.Text.Text implementation:Standard.Base.Any.Any -> Standard.Base.Network.Email.Email_Provider.Email_Provider
