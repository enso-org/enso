## Enso Signatures 1.0
## module Standard.Base.Data.Index_Sub_Range
- type Index_Sub_Range
    - By_Index indexes:(Standard.Base.Data.Numbers.Integer|Standard.Base.Data.Range.Range|Standard.Base.Any.Any)=
    - Every step:Standard.Base.Data.Numbers.Integer= first:Standard.Base.Data.Numbers.Integer=
    - First count:Standard.Base.Data.Numbers.Integer=
    - Last count:Standard.Base.Data.Numbers.Integer=
    - Sample count:Standard.Base.Data.Numbers.Integer= seed:Standard.Base.Data.Numbers.Integer=
    - While predicate:Standard.Base.Any.Any=
    - default_options count:(Standard.Base.Data.Numbers.Integer|Standard.Base.Nothing.Nothing)= -> Standard.Base.Any.Any
    - default_widget count:(Standard.Base.Data.Numbers.Integer|Standard.Base.Nothing.Nothing)= display:Standard.Base.Metadata.Display= -> Standard.Base.Metadata.Widget
    - to_display_text self -> Standard.Base.Any.Any
- drop_helper length:Standard.Base.Any.Any at:Standard.Base.Any.Any single_slice:Standard.Base.Any.Any slice_ranges:Standard.Base.Any.Any range:(Standard.Base.Data.Index_Sub_Range.Index_Sub_Range|Standard.Base.Data.Range.Range|Standard.Base.Data.Numbers.Integer) -> Standard.Base.Any.Any
- handle_unmatched_type expected_types:Standard.Base.Any.Any actual_value:Standard.Base.Any.Any -> Standard.Base.Any.Any
- invert_range_selection ranges:Standard.Base.Any.Any length:Standard.Base.Any.Any needs_sorting:Standard.Base.Any.Any -> Standard.Base.Any.Any
- normalize_ranges descriptors:Standard.Base.Any.Any -> Standard.Base.Any.Any
- resolve_ranges ranges:Standard.Base.Any.Any length:Standard.Base.Any.Any -> Standard.Base.Any.Any
- sort_and_merge_ranges ranges:Standard.Base.Any.Any -> Standard.Base.Any.Any
- take_helper length:Standard.Base.Any.Any at:Standard.Base.Any.Any single_slice:Standard.Base.Any.Any slice_ranges:Standard.Base.Any.Any range:(Standard.Base.Data.Index_Sub_Range.Index_Sub_Range|Standard.Base.Data.Range.Range|Standard.Base.Data.Numbers.Integer) -> Standard.Base.Any.Any
