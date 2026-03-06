## Enso Signatures 1.0
## module Standard.Table.Internal.Join_Helpers
- type Join_Condition_Resolution
    - Result conditions:(Standard.Base.Data.Vector.Vector Standard.Base.Any.Any) redundant_column_names:(Standard.Base.Data.Vector.Vector Standard.Base.Data.Text.Text)
- type Join_Condition_Resolver
    - Value left_at:Standard.Base.Any.Any right_at:Standard.Base.Any.Any make_equals:Standard.Base.Any.Any make_equals_ignore_case:Standard.Base.Any.Any make_between:Standard.Base.Any.Any make_custom_sql:Standard.Base.Any.Any
    - resolve self conditions:Standard.Base.Any.Any on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Base.Any.Any
