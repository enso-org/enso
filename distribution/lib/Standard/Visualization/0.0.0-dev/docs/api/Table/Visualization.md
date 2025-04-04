## Enso Signatures 1.0
## module Standard.Visualization.Table.Visualization
- apply_filter_to_table table:Standard.Base.Any.Any i:Standard.Base.Any.Any filter_cols:Standard.Base.Any.Any filter_conditions:Standard.Base.Any.Any -> Standard.Base.Any.Any
- apply_sort_to_table table:Standard.Base.Any.Any sort_col_index_list:Standard.Base.Any.Any sort_direction_list:Standard.Base.Any.Any -> Standard.Base.Any.Any
- get_distinct_values_for_column table:Standard.Base.Any.Any column_index:Standard.Base.Any.Any -> Standard.Base.Any.Any
- get_rows_for_table table:Standard.Base.Any.Any start_number:Standard.Base.Any.Any sort_col_index_list:Standard.Base.Any.Any= sort_direction_list:Standard.Base.Any.Any= filter_col:Standard.Base.Any.Any= filter_condition:Standard.Base.Any.Any= -> Standard.Base.Any.Any
- make_json_for_dictionary dict:Standard.Base.Any.Any max_items:Standard.Base.Any.Any -> Standard.Base.Any.Any
- make_json_for_error error:Standard.Base.Any.Any -> Standard.Base.Any.Any
- make_json_for_js_object js_object:Standard.Base.Any.Any max_items:Standard.Base.Any.Any -> Standard.Base.Any.Any
- make_json_for_matrix current:Standard.Base.Any.Any vector:Standard.Base.Any.Any idx:Standard.Base.Any.Any= -> Standard.Base.Any.Any
- make_json_for_object_matrix current:Standard.Base.Any.Any vector:Standard.Base.Any.Any idx:Standard.Base.Any.Any= -> Standard.Base.Any.Any
- make_json_for_other x:Standard.Base.Any.Any -> Standard.Base.Any.Any
- make_json_for_row row:Standard.Base.Any.Any -> Standard.Base.Any.Any
- make_json_for_table dataframe:Standard.Base.Any.Any max_rows:Standard.Base.Any.Any all_rows_count:Standard.Base.Any.Any is_db_table:Standard.Base.Any.Any -> Standard.Base.Any.Any
- make_json_for_value val:Standard.Base.Any.Any level:Standard.Base.Any.Any= -> Standard.Base.Any.Any
- make_json_for_vector vector:Standard.Base.Any.Any max_rows:Standard.Base.Any.Any -> Standard.Base.Any.Any
- make_json_for_xml_element xml_element:Standard.Base.Any.Any max_items:Standard.Base.Any.Any type:Standard.Base.Data.Text.Text= -> Standard.Base.Any.Any
- max_columns -> Standard.Base.Any.Any
- prepare_visualization y:Standard.Base.Any.Any max_rows:Standard.Base.Any.Any= -> Standard.Base.Any.Any
