## Enso Signatures 1.0
## module Standard.Database.Internal.Common.Database_Join_Helper
- type Join_Subquery_Setup
    - Value subquery:Standard.Database.Internal.IR.SQL_IR_From_Part.SQL_IR_From_Part new_columns:(Standard.Base.Data.Vector.Vector Standard.Database.Internal.IR.Internal_Column.Internal_Column) old_columns:(Standard.Base.Data.Vector.Vector Standard.Database.Internal.IR.Internal_Column.Internal_Column) indicator_column:(Standard.Base.Nothing.Nothing|Standard.Database.Internal.IR.Internal_Column.Internal_Column)
    - column_mapping self -> Standard.Base.Any.Any
- make_join_helpers left_table:Standard.Base.Any.Any right_table:Standard.Base.Any.Any left_column_mapping:Standard.Base.Any.Any right_column_mapping:Standard.Base.Any.Any -> Standard.Base.Any.Any
- prepare_subqueries connection:Standard.Base.Any.Any left:Standard.Base.Any.Any right:Standard.Base.Any.Any needs_left_indicator:Standard.Base.Any.Any needs_right_indicator:Standard.Base.Any.Any -> Standard.Base.Any.Any
- select_columns_for_join column_naming_helper:Standard.Base.Any.Any join_kind:Standard.Base.Any.Any left_new_columns:Standard.Base.Any.Any right_new_columns:Standard.Base.Any.Any right_columns_to_drop:Standard.Base.Any.Any right_prefix:Standard.Base.Any.Any problem_builder:Standard.Base.Any.Any -> Standard.Base.Any.Any
