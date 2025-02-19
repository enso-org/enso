## Enso Signatures 1.0
## module Standard.Base.Data.Text.Regex.Internal.Match_Iterator
- type Match_Iterator
    - Value pattern:Standard.Base.Data.Text.Regex.Regex input:Standard.Base.Data.Text.Text cursor:Standard.Base.Data.Numbers.Integer
    - early_exit self -> Standard.Base.Any.Any
    - new pattern:Standard.Base.Any.Any input:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - next self -> Standard.Base.Any.Any
- type Match_Iterator_Value
    - Last filler:Standard.Base.Data.Text.Span.Utf_16_Span
    - Next filler:Standard.Base.Data.Text.Span.Utf_16_Span match:Standard.Base.Data.Text.Regex.Match.Match next_iterator:Standard.Base.Data.Text.Regex.Internal.Match_Iterator.Match_Iterator
