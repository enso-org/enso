## Enso Signatures 1.0
## module Standard.Base.Network.Email.Providers.Send_Grid
- type Send_Grid_Provider
    - Send_Grid api_key:(Standard.Base.Data.Text.Text|Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret)=
    - get_dropdown_options -> Standard.Base.Any.Any
- Standard.Base.Network.Email.Email_Provider.Email_Provider.from that:Standard.Base.Network.Email.Providers.Send_Grid.Send_Grid_Provider -> Standard.Base.Network.Email.Email_Provider.Email_Provider
