## Enso Signatures 1.0
## module Standard.Base.Network.Email.Email_Send_Response
- type Send_Response
    - Dry_Run email:Standard.Base.Network.Email.Email
    - Failed email:Standard.Base.Network.Email.Email
    - Sent email:Standard.Base.Network.Email.Email
    - Test email:Standard.Base.Network.Email.Email
    - to_display_text self -> Standard.Base.Data.Text.Text
