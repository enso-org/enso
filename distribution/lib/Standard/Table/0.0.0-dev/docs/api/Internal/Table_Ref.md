## Enso Signatures 1.0
## module Standard.Table.Internal.Table_Ref
- type Table_Ref
    - Value underlying:Standard.Base.Any.Any
    - at self selector:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - column_names self -> Standard.Base.Any.Any
    - evaluate_expression self expression:Standard.Table.Expression.Expression on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior= -> Standard.Base.Any.Any
    - resolve self value:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - resolve_as_column self value:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - resolve_condition self condition:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - set self column:Standard.Base.Any.Any as:Standard.Base.Data.Text.Text set_mode:Standard.Table.Set_Mode.Set_Mode= on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior= -> Standard.Base.Any.Any
- check_is_in_values operation_name:Standard.Base.Any.Any values:Standard.Base.Any.Any -> Standard.Base.Any.Any
- Standard.Table.Internal.Table_Ref.Table_Ref.from that:Standard.Table.Table.Table -> Standard.Table.Internal.Table_Ref.Table_Ref
