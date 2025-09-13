## Enso Signatures 1.0
## module Standard.Table.Internal.Lookup_Helpers
- type Lookup_Column
    - Add_Column lookup_column:Standard.Base.Any.Any
    - Keep_Column base_column:Standard.Base.Any.Any
    - Key_Column base_column:Standard.Base.Any.Any lookup_column:Standard.Base.Any.Any
    - Replace_Column base_column:Standard.Base.Any.Any lookup_column:Standard.Base.Any.Any common_type:Standard.Table.Value_Type.Value_Type
    - is_key self -> Standard.Base.Any.Any
- make_java_lookup_column_description lookup_column:Standard.Base.Any.Any -> Standard.Base.Any.Any
- merge_columns base_column:Standard.Base.Any.Any lookup_column:Standard.Base.Any.Any allow_unmatched_rows:Standard.Base.Any.Any -> Standard.Base.Any.Any
- prepare_columns_for_lookup base_table:Standard.Base.Any.Any lookup_table:Standard.Base.Any.Any key_columns_selector:Standard.Base.Any.Any add_new_columns:Standard.Base.Any.Any allow_unmatched_rows:Standard.Base.Any.Any on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Base.Any.Any
