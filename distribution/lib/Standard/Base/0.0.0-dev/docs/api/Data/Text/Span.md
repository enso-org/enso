## Enso Signatures 1.0
## module Standard.Base.Data.Text.Span
- type Span
    - Value range:Standard.Base.Data.Range.Range parent:Standard.Base.Data.Text.Text
    - end self -> Standard.Base.Any.Any
    - length self -> Standard.Base.Any.Any
    - start self -> Standard.Base.Any.Any
    - text self -> Standard.Base.Any.Any
    - to_display_text self -> Standard.Base.Any.Any
    - to_utf_16_span self -> Standard.Base.Any.Any
- type Utf_16_Span
    - Value range:Standard.Base.Data.Range.Range parent:Standard.Base.Data.Text.Text
    - end self -> Standard.Base.Any.Any
    - length self -> Standard.Base.Any.Any
    - start self -> Standard.Base.Any.Any
    - text self -> Standard.Base.Any.Any
    - to_display_text self -> Standard.Base.Any.Any
    - to_grapheme_span self -> Standard.Base.Any.Any
- range_to_char_indices text:Standard.Base.Any.Any range:Standard.Base.Any.Any -> Standard.Base.Any.Any
