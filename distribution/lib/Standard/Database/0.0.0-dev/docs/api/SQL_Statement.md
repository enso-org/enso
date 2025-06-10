## Enso Signatures 1.0
## module Standard.Database.SQL_Statement
- type SQL_Statement
    - deserialize json:(Standard.Base.Data.Text.Text|Standard.Base.Data.Json.JS_Object) -> Standard.Database.SQL_Statement.SQL_Statement!Standard.Base.Errors.Illegal_Argument.Illegal_Argument
    - fragments self -> Standard.Base.Any.Any
    - prepare self -> Standard.Base.Any.Any
    - serialize self ensure_roundtrip:Standard.Base.Data.Boolean.Boolean -> Standard.Base.Data.Json.JS_Object!Standard.Database.SQL_Statement.Unable_To_Serialize_SQL_Statement
    - to_js_object self -> Standard.Base.Any.Any
    - to_text self -> Standard.Base.Data.Text.Text
    - unsafe_to_raw_sql self -> Standard.Base.Any.Any
- type Unable_To_Serialize_SQL_Statement
    - Error obj:Standard.Base.Any.Any
    - to_display_text self -> Standard.Base.Any.Any
