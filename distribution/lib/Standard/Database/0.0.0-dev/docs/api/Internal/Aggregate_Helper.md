## Enso Signatures 1.0
## module Standard.Database.Internal.Aggregate_Helper
- type Aggregate_With_Helper_Expressions
    - Value create_helper_expressions:Standard.Base.Any.Any make_aggregate:Standard.Base.Any.Any
    - build self dialect:Standard.Base.Any.Any base_table:Standard.Base.Any.Any key_columns:Standard.Base.Any.Any resolved_aggregates:Standard.Base.Any.Any problem_builder:Standard.Base.Any.Any -> Standard.Base.Any.Any
- aggregate table:Standard.Database.DB_Table.DB_Table group_by:(Standard.Base.Data.Vector.Vector|Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer|Standard.Base.Data.Text.Regex.Regex) columns:Standard.Base.Data.Vector.Vector error_on_missing_columns:Standard.Base.Data.Boolean.Boolean on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Base.Any.Any
- default_build_aggregate build_aggregate:Standard.Base.Any.Any dialect:Standard.Base.Any.Any base_table:Standard.Base.Any.Any key_columns:Standard.Base.Any.Any resolved_aggregates:Standard.Base.Any.Any problem_builder:Standard.Base.Any.Any -> Standard.Base.Any.Any
- is_non_empty_selector v:Standard.Base.Any.Any -> Standard.Base.Any.Any
- make_aggregate_column table:Standard.Base.Any.Any aggregate:Standard.Base.Any.Any as:Standard.Base.Any.Any dialect:Standard.Base.Any.Any infer_return_type:Standard.Base.Any.Any problem_builder:Standard.Base.Any.Any -> Standard.Database.Internal.IR.Internal_Column.Internal_Column
- make_infer_return_type connection:Standard.Base.Any.Any context:Standard.Base.Any.Any -> Standard.Base.Any.Any
- map_column_inputs f:Standard.Base.Function.Function aggregate_column:Standard.Table.Aggregate_Column.Aggregate_Column -> Standard.Table.Aggregate_Column.Aggregate_Column
- throw_ordering_required op_name:Standard.Base.Any.Any -> Standard.Base.Any.Any
