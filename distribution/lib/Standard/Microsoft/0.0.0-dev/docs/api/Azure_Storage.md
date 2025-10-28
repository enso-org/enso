## Enso Signatures 1.0
## module Standard.Microsoft.Azure_Storage
- type Azure_Blob_Storage
    - SAS_Token token:(Standard.Base.Data.Text.Text|Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret)=
    - default_widget display:Standard.Base.Metadata.Display= -> Standard.Base.Metadata.Widget
- type Azure_Storage
    - accounts environment:Standard.Microsoft.Azure_Environment.Azure_Environment= cred:Standard.Microsoft.Azure_Credential.Azure_Credential= subscription_id:(Standard.Base.Data.Text.Text|Standard.Base.Nothing.Nothing)= -> (Standard.Base.Data.Vector.Vector Standard.Microsoft.Azure_Storage.Azure_Storage_Account)
    - blob_containers account:Standard.Base.Data.Text.Text= prefix:Standard.Base.Data.Text.Text= cred:(Standard.Microsoft.Azure_Credential.Azure_Credential|Standard.Microsoft.Azure_Storage.Azure_Blob_Storage)= -> (Standard.Base.Data.Vector.Vector Standard.Base.Data.Text.Text)
    - get_blob account:Standard.Base.Data.Text.Text= container:Standard.Base.Data.Text.Text= blob:Standard.Base.Data.Text.Text= cred:(Standard.Microsoft.Azure_Credential.Azure_Credential|Standard.Microsoft.Azure_Storage.Azure_Blob_Storage)= -> Standard.Base.System.File.File
    - list_blob account:Standard.Base.Data.Text.Text= container:Standard.Base.Data.Text.Text= prefix:Standard.Base.Data.Text.Text= cred:(Standard.Microsoft.Azure_Credential.Azure_Credential|Standard.Microsoft.Azure_Storage.Azure_Blob_Storage)= -> (Standard.Base.Data.Vector.Vector Standard.Base.Data.Text.Text)
- type Azure_Storage_Account
    - endpoints self -> Standard.Base.Data.Text.Text
    - id self -> Standard.Base.Data.Text.Text
    - name self -> Standard.Base.Data.Text.Text
    - resource_group self -> Standard.Base.Data.Text.Text
    - to_display_text self -> Standard.Base.Data.Text.Text
    - to_js_object self -> Standard.Base.Any.Any
    - to_text self -> Standard.Base.Data.Text.Text
