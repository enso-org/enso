## Enso Signatures 1.0
## module Standard.Base.Data.Text.Text_Sub_Range
- type Codepoint_Ranges
    - Value ranges:(Standard.Base.Data.Vector.Vector Standard.Base.Data.Range.Range) is_sorted_and_distinct:Standard.Base.Data.Boolean.Boolean
    - resolve text:Standard.Base.Data.Text.Text range:(Standard.Base.Data.Text.Text_Sub_Range.Text_Sub_Range|Standard.Base.Data.Index_Sub_Range.Index_Sub_Range|Standard.Base.Data.Range.Range|Standard.Base.Data.Numbers.Integer) -> Standard.Base.Any.Any
    - sorted_and_distinct_ranges self -> Standard.Base.Any.Any
- type Text_Sub_Range
    - After delimiter:Standard.Base.Data.Text.Text
    - After_Last delimiter:Standard.Base.Data.Text.Text
    - Before delimiter:Standard.Base.Data.Text.Text
    - Before_Last delimiter:Standard.Base.Data.Text.Text
    - default_options -> Standard.Base.Any.Any
    - default_widget display:Standard.Base.Metadata.Display= -> Standard.Base.Metadata.Widget
    - to_display_text self -> Standard.Base.Any.Any
- batch_resolve_indices_or_ranges text:Standard.Base.Any.Any descriptors:Standard.Base.Any.Any -> Standard.Base.Any.Any
- character_ranges text:Standard.Base.Any.Any -> Standard.Base.Any.Any
- find_sub_range_end text:Standard.Base.Any.Any predicate:Standard.Base.Any.Any -> Standard.Base.Any.Any
- normalize_range range:Standard.Base.Any.Any length:Standard.Base.Any.Any -> Standard.Base.Any.Any
- panic_on_non_positive_step -> Standard.Base.Any.Any
- resolve_index_or_range text:Standard.Base.Any.Any descriptor:Standard.Base.Any.Any -> Standard.Base.Any.Any
