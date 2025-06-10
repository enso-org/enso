## Enso Signatures 1.0
## module Standard.Base.Data.Text
- type Text
    - + self that:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - compare_to_ignore_case self that:Standard.Base.Any.Any locale:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - equals_ignore_case self that:Standard.Base.Data.Text.Text locale:Standard.Base.Data.Locale.Locale= -> Standard.Base.Any.Any
    - is_empty self -> Standard.Base.Any.Any
    - is_normalized self -> Standard.Base.Any.Any
    - length self -> Standard.Base.Any.Any
    - normalize self normalization:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - not_empty self -> Standard.Base.Any.Any
    - to_case_insensitive_key self locale:Standard.Base.Data.Locale.Locale= -> Standard.Base.Any.Any
    - to_text self -> Standard.Base.Any.Any
