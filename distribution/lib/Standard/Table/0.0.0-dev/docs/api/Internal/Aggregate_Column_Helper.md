## Enso Signatures 1.0
## module Standard.Table.Internal.Aggregate_Column_Helper
- type Internal_Missing_Column_Error
- type Internal_Order_By_Column_Reference
    - Value column:Standard.Base.Any.Any direction:Standard.Base.Any.Any
- type Validated_Aggregate_Columns
    - Value key_columns:(Standard.Base.Data.Vector.Vector Standard.Table.Column.Column) valid_columns:(Standard.Base.Data.Vector.Vector Standard.Base.Any.Any) problems:(Standard.Base.Data.Vector.Vector Standard.Base.Any.Any) old_style:Standard.Base.Data.Boolean.Boolean
- all_same_column aggregates:Standard.Base.Any.Any -> Standard.Base.Any.Any
- default_aggregate_column_name aggregate_column:Standard.Base.Any.Any include_column:Standard.Base.Any.Any= -> Standard.Base.Any.Any
- java_aggregator name:Standard.Base.Any.Any column:Standard.Base.Any.Any -> Standard.Base.Any.Any
- prepare_aggregate_columns naming_helper:Standard.Base.Any.Any group_by:Standard.Base.Any.Any aggregates:Standard.Base.Any.Any table:Standard.Base.Any.Any error_on_missing_columns:Standard.Base.Any.Any -> Standard.Base.Any.Any
- resolve_aggregate table:Standard.Base.Any.Any problem_builder:Standard.Base.Any.Any aggregate_column:Standard.Table.Aggregate_Column.Aggregate_Column -> Standard.Base.Any.Any
