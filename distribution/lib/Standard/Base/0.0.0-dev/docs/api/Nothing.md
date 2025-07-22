## Enso Signatures 1.0
## module Standard.Base.Nothing
- type Nothing
    - get self key:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)= ~if_missing:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - if_not_nothing self ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - if_nothing self ~function:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - is_nothing self -> Standard.Base.Any.Any
