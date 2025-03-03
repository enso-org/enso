## Enso Signatures 1.0
## module Standard.Table.Internal.Split_Tokenize
- handle_nothing function:Standard.Base.Any.Any x:Standard.Base.Any.Any -> Standard.Base.Any.Any
- parse_to_columns table:Standard.Base.Any.Any input_column_id:Standard.Base.Any.Any pattern:(Standard.Base.Data.Text.Text|Standard.Base.Data.Text.Regex.Regex)= case_sensitivity:Standard.Base.Any.Any= parse_values:Standard.Base.Any.Any= on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior= -> Standard.Base.Any.Any
- regex_parse_to_vectors pattern:Standard.Base.Any.Any input:Standard.Base.Any.Any -> Standard.Base.Any.Any
- regex_to_column_names pattern:Standard.Base.Any.Any original_column_name:Standard.Base.Any.Any -> Standard.Base.Any.Any
- split_to_columns table:Standard.Base.Any.Any input_column_id:Standard.Base.Any.Any delimiter:Standard.Base.Any.Any= column_count:Standard.Table.Columns_To_Add.Columns_To_Add= on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior= -> Standard.Base.Any.Any
- split_to_rows table:Standard.Base.Any.Any input_column_id:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer) delimiter:Standard.Base.Any.Any= -> Standard.Base.Any.Any
- tokenize_to_columns table:Standard.Base.Any.Any input_column_id:Standard.Base.Any.Any pattern:Standard.Base.Any.Any case_sensitivity:Standard.Base.Any.Any column_count:Standard.Base.Any.Any on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Base.Any.Any
- tokenize_to_rows table:Standard.Base.Any.Any input_column_id:Standard.Base.Any.Any pattern:Standard.Base.Any.Any= case_sensitivity:Standard.Base.Any.Any= at_least_one_row:Standard.Base.Any.Any= -> Standard.Base.Any.Any
