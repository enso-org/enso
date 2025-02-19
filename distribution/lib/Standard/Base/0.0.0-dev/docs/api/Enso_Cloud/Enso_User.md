## Enso Signatures 1.0
## module Standard.Base.Enso_Cloud.Enso_User
- type Enso_User
    - User name:Standard.Base.Data.Text.Text email:Standard.Base.Data.Text.Text id:Standard.Base.Data.Text.Text is_enabled:Standard.Base.Data.Boolean.Boolean root_directory_id:Standard.Base.Data.Text.Text organization_name:Standard.Base.Data.Text.Text
    - current -> Standard.Base.Enso_Cloud.Enso_User.Enso_User
    - flush_caches -> Standard.Base.Any.Any
    - is_logged_in -> Standard.Base.Data.Boolean.Boolean
    - list -> Standard.Base.Any.Any
    - to_display_text self -> Standard.Base.Any.Any
    - to_text self -> Standard.Base.Any.Any
- Standard.Base.Enso_Cloud.Enso_User.Enso_User.from that:Standard.Base.Data.Json.JS_Object -> Standard.Base.Enso_Cloud.Enso_User.Enso_User
