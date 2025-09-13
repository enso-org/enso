## Enso Signatures 1.0
## module Standard.Microsoft.Azure
- type Azure
    - login cred:Standard.Microsoft.Azure_Credential.Azure_Credential= -> Standard.Microsoft.Azure_Credential.Azure_Credential
    - subscriptions cred:Standard.Microsoft.Azure_Credential.Azure_Credential= environment:Standard.Microsoft.Azure_Environment.Azure_Environment= -> (Standard.Base.Data.Vector.Vector Standard.Microsoft.Azure_Credential.Azure_Subscription)
    - tenants cred:Standard.Microsoft.Azure_Credential.Azure_Credential= environment:Standard.Microsoft.Azure_Environment.Azure_Environment= -> (Standard.Base.Data.Vector.Vector Standard.Microsoft.Azure_Credential.Azure_Tenant)
