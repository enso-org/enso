## Enso Signatures 1.0
## module Standard.Base.Data.Text.Regex.Match
- type Match
    - Value pattern:Standard.Base.Data.Text.Regex.Regex internal_regex_result:Standard.Base.Any.Any input:Standard.Base.Data.Text.Text
    - at self index:Standard.Base.Data.Numbers.Integer= -> Standard.Base.Any.Any
    - end self group:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - get self index:Standard.Base.Data.Numbers.Integer= ~if_missing:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - groups self ~default:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - internal_end self group:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - internal_start self group:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - named_groups self default:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - span self group:Standard.Base.Any.Any= ~default:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - start self group:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - text self group:Standard.Base.Any.Any= ~default:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - to_display_text self -> Standard.Base.Any.Any
    - to_js_object self -> Standard.Base.Any.Any
    - utf_16_end self group:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - utf_16_span self group:Standard.Base.Any.Any= ~default:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - utf_16_start self group:Standard.Base.Any.Any= -> Standard.Base.Any.Any
