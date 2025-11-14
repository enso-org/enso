## Enso Signatures 1.0
## module Standard.Base.Network.Email.Email_Provider
- type Email_Provider
    - SendGrid api_key:(Standard.Base.Data.Text.Text|Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret)=
    - TestOnly
    - send self email:Standard.Base.Network.Email.Email_Object.Email_Object -> Standard.Base.Any.Any
