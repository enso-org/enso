## Enso Signatures 1.0
## module Standard.Table.Internal.Filter_Condition_Helpers
- make_filter_column source_column:Standard.Table.Column.Column filter_condition:Standard.Base.Any.Any on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Table.Column.Column
- warn_on_nothing_in_comparison filter_condition:Standard.Base.Any.Any value:Standard.Base.Any.Any ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
- warn_on_nothing_in_comparison_vector filter_condition:Standard.Base.Any.Any values:Standard.Base.Any.Any ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
