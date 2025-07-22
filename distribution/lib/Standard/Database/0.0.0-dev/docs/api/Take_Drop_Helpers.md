## Enso Signatures 1.0
## module Standard.Database.Take_Drop_Helpers
- type Take_Drop
    - Drop
    - Take
- check_supported selector:(Standard.Base.Data.Index_Sub_Range.Index_Sub_Range|Standard.Base.Data.Range.Range|Standard.Base.Data.Numbers.Integer) ~cont:Standard.Base.Any.Any -> Standard.Base.Any.Any
- cleanup_ranges ranges:(Standard.Base.Data.Vector.Vector Standard.Base.Data.Range.Range) -> Standard.Base.Any.Any
- collect_ranges take_drop:Standard.Base.Any.Any length:Standard.Base.Any.Any selector:(Standard.Base.Data.Index_Sub_Range.Index_Sub_Range|Standard.Base.Data.Range.Range|Standard.Base.Data.Numbers.Integer) -> Standard.Base.Any.Any
- generate_subquery table:(Standard.Table.Table.Table&Standard.Database.DB_Table.DB_Table) row_column_name:Standard.Base.Any.Any range:Standard.Base.Any.Any -> Standard.Base.Any.Any
- take_drop_helper take_drop:Standard.Base.Any.Any table:Standard.Base.Any.Any selector:(Standard.Base.Data.Index_Sub_Range.Index_Sub_Range|Standard.Base.Data.Range.Range|Standard.Base.Data.Numbers.Integer) -> Standard.Base.Any.Any
