## Enso Signatures 1.0
## module Standard.Saas.Email_Provider_SMTP
- type Email_Provider_SMTP
    - SMTP server:Standard.Base.Data.Text.Text= username:Standard.Base.Data.Text.Text= password:(Standard.Base.Data.Text.Text|Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret)= port:Standard.Base.Data.Numbers.Integer= use_tls:Standard.Base.Data.Boolean.Boolean=
    - get_dropdown_options -> Standard.Base.Any.Any
    - resolve that:Standard.Base.Function.Function -> (Standard.Saas.Email_Provider_SMTP.Email_Provider_SMTP|Standard.Base.Nothing.Nothing)
- Standard.Base.Network.Email.Email_Provider.Email_Provider.from that:Standard.Saas.Email_Provider_SMTP.Email_Provider_SMTP -> Standard.Base.Network.Email.Email_Provider.Email_Provider
