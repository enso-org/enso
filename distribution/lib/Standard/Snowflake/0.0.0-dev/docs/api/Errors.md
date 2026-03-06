## Enso Signatures 1.0
## module Standard.Snowflake.Errors
- type File_Format_Not_Found
    - Error name:Standard.Base.Data.Text.Text
    - to_display_text self -> Standard.Base.Any.Any
- type Snowflake_Error
    - error_message self -> Standard.Base.Data.Text.Text
    - payload self -> Standard.Base.Any.Any
    - to_display_text self -> Standard.Base.Data.Text.Text
    - to_js_object self -> Standard.Base.Data.Json.JS_Object
- type Stage_Not_Found
    - Error name:Standard.Base.Data.Text.Text
    - to_display_text self -> Standard.Base.Any.Any
