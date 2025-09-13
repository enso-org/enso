## Enso Signatures 1.0
## module Standard.Microsoft.Azure_Credential
- type Azure_Credential
    - CLI
    - Client_Secret tenant_id:(Standard.Base.Data.Text.Text|Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret)= client_id:(Standard.Base.Data.Text.Text|Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret)= client_secret:(Standard.Base.Data.Text.Text|Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret)=
    - Default
    - Environment
    - subscriptions self environment:Standard.Microsoft.Azure_Environment.Azure_Environment= -> (Standard.Base.Data.Vector.Vector Standard.Microsoft.Azure_Credential.Azure_Subscription)
    - tenants self environment:Standard.Microsoft.Azure_Environment.Azure_Environment= -> (Standard.Base.Data.Vector.Vector Standard.Microsoft.Azure_Credential.Azure_Tenant)
- type Azure_Subscription
    - id self -> Standard.Base.Data.Text.Text
    - name self -> Standard.Base.Data.Text.Text
    - to_display_text self -> Standard.Base.Data.Text.Text
    - to_js_object self -> Standard.Base.Data.Json.JS_Object
    - to_text self -> Standard.Base.Data.Text.Text
- type Azure_Tenant
    - Value id:Standard.Base.Any.Any
    - to_display_text self -> Standard.Base.Data.Text.Text
- Standard.Base.Data.Text.Text.from that:Standard.Microsoft.Azure_Credential.Azure_Subscription -> Standard.Base.Data.Text.Text
