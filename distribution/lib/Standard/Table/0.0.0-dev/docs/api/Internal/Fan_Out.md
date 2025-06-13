## Enso Signatures 1.0
## module Standard.Table.Internal.Fan_Out
- fan_out_to_columns table:Standard.Base.Any.Any input_column_id:Standard.Base.Any.Any function:Standard.Base.Any.Any column_count:Standard.Base.Any.Any column_builder:Standard.Base.Any.Any= on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior= -> Standard.Base.Any.Any
- fan_out_to_rows table:Standard.Base.Any.Any input_column_id:Standard.Base.Data.Text.Text function:Standard.Base.Any.Any column_names:Standard.Base.Any.Any= at_least_one_row:Standard.Base.Any.Any= column_builder:Standard.Base.Any.Any= on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior= -> Standard.Base.Any.Any
- fan_out_to_rows_and_columns table:Standard.Base.Any.Any input_column_id:Standard.Base.Any.Any function:Standard.Base.Any.Any column_names:Standard.Base.Any.Any at_least_one_row:Standard.Base.Any.Any= column_builder:Standard.Base.Any.Any= on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior= -> Standard.Base.Any.Any
- fan_out_to_rows_and_columns_dynamic input_java_column:Standard.Base.Any.Any function:Standard.Base.Any.Any at_least_one_row:Standard.Base.Any.Any column_names_for_row:Standard.Base.Any.Any column_builder:Standard.Base.Any.Any problem_builder:Standard.Base.Any.Any -> Standard.Base.Any.Any
- fan_out_to_rows_and_columns_fixed input_java_column:Standard.Base.Any.Any function:Standard.Base.Any.Any at_least_one_row:Standard.Base.Data.Boolean.Boolean column_names:Standard.Base.Data.Vector.Vector column_builder:Standard.Base.Any.Any problem_builder:Standard.Base.Any.Any -> Standard.Base.Any.Any
- map_columns_to_multiple input_column:Standard.Base.Any.Any function:Standard.Base.Any.Any column_count:Standard.Base.Any.Any column_builder:Standard.Base.Any.Any= problem_builder:Standard.Base.Any.Any -> Standard.Base.Any.Any
- maximum vec:Standard.Base.Any.Any -> Standard.Base.Any.Any
- rename_new_columns table:Standard.Base.Any.Any removed_column_name:Standard.Base.Any.Any columns:Standard.Base.Any.Any problem_builder:Standard.Base.Any.Any -> Standard.Base.Any.Any
- repeat_each n:Standard.Base.Any.Any ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
- replace_column_with_columns table:Standard.Base.Any.Any old_column:Standard.Base.Any.Any new_columns:Standard.Base.Any.Any -> Standard.Base.Any.Any
- uniform_length target_length:Standard.Base.Any.Any v:Standard.Base.Any.Any problem_builder:Standard.Base.Any.Any -> Standard.Base.Any.Any
