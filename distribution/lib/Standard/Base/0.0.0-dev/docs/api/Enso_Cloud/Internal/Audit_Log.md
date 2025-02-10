## Enso Signatures 1.0
## module Standard.Base.Enso_Cloud.Internal.Audit_Log
- type Audit_Log
    - report_event event_type:Standard.Base.Data.Text.Text message:Standard.Base.Data.Text.Text metadata:Standard.Base.Data.Json.JS_Object= async:Standard.Base.Data.Boolean.Boolean= -> Standard.Base.Nothing.Nothing
- type Audit_Log_Error
    - Error message:Standard.Base.Data.Text.Text cause:Standard.Base.Any.Any
    - handle_java_exception -> Standard.Base.Any.Any
