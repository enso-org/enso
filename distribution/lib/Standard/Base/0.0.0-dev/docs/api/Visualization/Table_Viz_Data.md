## Enso Signatures 1.0
## module Standard.Base.Visualization.Table_Viz_Data
- type Table_Viz_Data
    - Error error_display_text:Standard.Base.Any.Any
    - GenericGrid headers:Standard.Base.Any.Any data:Standard.Base.Any.Any
    - SingleColumnOfActions js_value:Standard.Base.Any.Any data:Standard.Base.Any.Any title:Standard.Base.Any.Any tooltip:Standard.Base.Any.Any child_node_action:Standard.Base.Any.Any
    - Value json:Standard.Base.Data.Json.JS_Object
    - get_json self -> Standard.Base.Data.Json.JS_Object
- type Table_Viz_Header
    - Label name:Standard.Base.Any.Any
    - Link name:Standard.Base.Any.Any tooltip:Standard.Base.Any.Any action:Standard.Base.Any.Any
