## Enso Signatures 1.0
## module Standard.Base.Data.Maybe
- type Maybe
    - None
    - Some value:Standard.Base.Any.Any
    - is_none self -> Standard.Base.Any.Any
    - is_some self -> Standard.Base.Any.Any
    - maybe self ~default:Standard.Base.Any.Any function:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - to_display_text self -> Standard.Base.Any.Any
