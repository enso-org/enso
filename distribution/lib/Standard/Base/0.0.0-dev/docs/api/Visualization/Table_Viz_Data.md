## Enso Signatures 1.0
## module Standard.Base.Visualization.Table_Viz_Data
- type Table_Viz_Data
    - Error error_display_text:Standard.Base.Any.Any
    - GenericGrid headers:Standard.Base.Any.Any data:Standard.Base.Any.Any
    - Value js_object:Standard.Base.Data.Json.JS_Object
    - get_js_object self -> Standard.Base.Data.Json.JS_Object
- type Table_Viz_Header
    - Label name:Standard.Base.Any.Any
    - Link name:Standard.Base.Any.Any tooltip:Standard.Base.Any.Any action:Standard.Base.Any.Any
