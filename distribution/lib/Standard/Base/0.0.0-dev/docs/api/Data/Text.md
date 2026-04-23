## Enso Signatures 1.0
## module Standard.Base.Data.Text
- type Text
    - + self that:Standard.Base.Data.Text.Text -> Standard.Base.Data.Text.Text
    - is_empty self -> Standard.Base.Any.Any
    - is_normalized self -> Standard.Base.Data.Boolean.Boolean
    - length self -> Standard.Base.Data.Numbers.Integer
    - normalize self normalization:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - not_empty self -> Standard.Base.Any.Any
    - to_text self -> Standard.Base.Any.Any
