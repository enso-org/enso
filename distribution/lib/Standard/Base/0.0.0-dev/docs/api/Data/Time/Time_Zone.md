## Enso Signatures 1.0
## module Standard.Base.Data.Time.Time_Zone
- type Time_Zone
    - default_widget display:Standard.Base.Metadata.Display= -> Standard.Base.Metadata.Widget
    - local -> Standard.Base.Any.Any
    - new hours:Standard.Base.Data.Numbers.Integer= minutes:Standard.Base.Data.Numbers.Integer= seconds:Standard.Base.Data.Numbers.Integer= -> Standard.Base.Any.Any
    - offset self at:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - parse id:Standard.Base.Data.Text.Text -> Standard.Base.Any.Any
    - pretty self -> Standard.Base.Any.Any
    - system -> Standard.Base.Any.Any
    - to_display_text self -> Standard.Base.Any.Any
    - to_js_object self -> Standard.Base.Any.Any
    - utc -> Standard.Base.Any.Any
    - zone_id self -> Standard.Base.Any.Any
    - zone_names -> Standard.Base.Any.Any
- new_builtin hours:Standard.Base.Any.Any minutes:Standard.Base.Any.Any seconds:Standard.Base.Any.Any -> Standard.Base.Any.Any
- parse_builtin text:Standard.Base.Any.Any -> Standard.Base.Any.Any
- Standard.Base.Data.Time.Time_Zone.Time_Zone.from that:Standard.Base.Data.Json.JS_Object -> Standard.Base.Data.Time.Time_Zone.Time_Zone
