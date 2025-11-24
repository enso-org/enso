## Enso Signatures 1.0
## module Standard.Base.Network.Email.Email_Provider
- type Email_Provider
    - Send_Grid api_key:(Standard.Base.Data.Text.Text|Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret)=
    - Test_Only
    - send self email:Standard.Base.Network.Email.Email -> Standard.Base.Any.Any
