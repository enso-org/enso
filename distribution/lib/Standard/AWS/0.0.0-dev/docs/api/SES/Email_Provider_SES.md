## Enso Signatures 1.0
## module Standard.AWS.SES.Email_Provider_SES
- type Email_Provider_SES
    - Amazon_SES region:Standard.AWS.AWS_Region.AWS_Region= credentials:Standard.AWS.AWS_Credential.AWS_Credential=
    - get_dropdown_options -> Standard.Base.Any.Any
    - resolve that:Standard.Base.Function.Function -> (Standard.AWS.SES.Email_Provider_SES.Email_Provider_SES|Standard.Base.Nothing.Nothing)
- Standard.Base.Network.Email.Email_Provider.Email_Provider.from that:Standard.AWS.SES.Email_Provider_SES.Email_Provider_SES -> Standard.Base.Network.Email.Email_Provider.Email_Provider
