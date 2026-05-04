## Enso Signatures 1.0
## module Standard.Base.Nothing
- type Nothing
    - if_not_nothing self ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - if_nothing self ~function:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - is_nothing self -> Standard.Base.Any.Any
- Standard.Base.Any.Any.if_not_nothing self ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
- Standard.Base.Any.Any.if_nothing self ~other:Standard.Base.Any.Any -> Standard.Base.Any.Any
- Standard.Base.Any.Any.is_nothing self -> Standard.Base.Data.Boolean.Boolean
- Standard.Base.Any.Any.map_nothing self f:Standard.Base.Any.Any -> Standard.Base.Any.Any
