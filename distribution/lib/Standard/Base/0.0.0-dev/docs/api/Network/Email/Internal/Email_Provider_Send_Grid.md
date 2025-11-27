## Enso Signatures 1.0
## module Standard.Base.Network.Email.Internal.Email_Provider_Send_Grid
- type Email_Provider_Send_Grid
    - Send_Grid api_key:(Standard.Base.Data.Text.Text|Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret)=
    - get_dropdown_options -> Standard.Base.Any.Any
    - resolve value:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - send self email:Standard.Base.Network.Email.Email -> Standard.Base.Any.Any
- type Email_Provider_Send_Grid_Impl
- Standard.Base.Network.Email.Email_Provider_SPI.Email_Provider_SPI.from that:Standard.Base.Network.Email.Internal.Email_Provider_Send_Grid.Email_Provider_Send_Grid_Impl -> Standard.Base.Network.Email.Email_Provider_SPI.Email_Provider_SPI
