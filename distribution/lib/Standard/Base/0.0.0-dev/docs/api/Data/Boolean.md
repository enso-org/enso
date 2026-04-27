## Enso Signatures 1.0
## module Standard.Base.Data.Boolean
- type Boolean
    - False
    - True
    - && self ~that:Standard.Base.Data.Boolean.Boolean -> Standard.Base.Data.Boolean.Boolean
    - if_then self ~on_true:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - if_then_else self ~on_true:Standard.Base.Any.Any ~on_false:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - not self -> Standard.Base.Data.Boolean.Boolean
    - || self ~that:Standard.Base.Data.Boolean.Boolean -> Standard.Base.Data.Boolean.Boolean
