## Enso Signatures 1.0
## module Standard.Base.Network.Email.Email_Object
- type Email_Object
    - Value from_address:Standard.Base.Network.Email.Email_Address.Email_Address to:(Standard.Base.Data.Vector.Vector Standard.Base.Network.Email.Email_Address.Email_Address)= cc:(Standard.Base.Data.Vector.Vector Standard.Base.Network.Email.Email_Address.Email_Address)= bcc:(Standard.Base.Data.Vector.Vector Standard.Base.Network.Email.Email_Address.Email_Address)= subject:Standard.Base.Data.Text.Text= body:Standard.Base.Network.Email.Email_Body.Email_Body= attachments:(Standard.Base.Data.Vector.Vector Standard.Base.Network.Email.Email_Attachment.Email_Attachment)=
    - to_text self -> Standard.Base.Data.Text.Text
