## Enso Signatures 1.0
## module Standard.Base.Network.Email.Email_Attachment
- type Email_Attachment
    - File file:(Standard.Base.Data.Text.Text|Standard.Base.Network.URI.URI|Standard.Base.System.File.File|Standard.Base.System.File.Generic.File_Like.File_Like)= name:Standard.Base.Data.Text.Text=
    - default_widget display:Standard.Base.Metadata.Display= -> Standard.Base.Metadata.Widget
- Standard.Base.Network.Email.Email_Attachment.Email_Attachment.from that:Standard.Base.System.File.File -> Standard.Base.Network.Email.Email_Attachment.Email_Attachment
- Standard.Base.Network.Email.Email_Attachment.Email_Attachment.from that:Standard.Base.Network.URI.URI -> Standard.Base.Network.Email.Email_Attachment.Email_Attachment
- Standard.Base.Network.Email.Email_Attachment.Email_Attachment.from that:Standard.Base.Data.Text.Text -> Standard.Base.Network.Email.Email_Attachment.Email_Attachment
