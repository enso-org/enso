## Enso Signatures 1.0
## module Standard.Base.Data.Text.Case_Sensitivity
- type Case_Sensitivity
    - Default
    - Insensitive locale:Standard.Base.Data.Locale.Locale=
    - Sensitive
    - disallow_non_default_locale self ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - folding_strategy case_sensitivity:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - is_case_insensitive_in_memory self -> Standard.Base.Any.Any
    - to_display_text self -> Standard.Base.Any.Any
    - to_explicit_sensitivity_in_memory self -> Standard.Base.Any.Any
