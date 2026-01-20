## Enso Signatures 1.0
## module Standard.Database.Internal.Common.Lookup_Query_Helper
- type Lookup_Subquery_Setup
    - Value self_sub:Standard.Base.Any.Any lookup_sub:Standard.Base.Any.Any lookup_counter:Standard.Base.Any.Any new_table_name:Standard.Base.Any.Any
    - create_merged_column self ix:Standard.Base.Any.Any expected_type:Standard.Base.Any.Any dialect:Standard.Base.Any.Any infer_type_in_result:Standard.Base.Any.Any allow_unmatched_rows:Standard.Base.Any.Any mapping:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - get_lookup_column self ix:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - get_self_column self ix:Standard.Base.Any.Any -> Standard.Base.Any.Any
- build_lookup_query base_table:Standard.Base.Any.Any lookup_table:Standard.Base.Any.Any key_columns:Standard.Base.Any.Any add_new_columns:Standard.Base.Any.Any allow_unmatched_rows:Standard.Base.Any.Any on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Base.Any.Any
- check_initial_invariants base_table:Standard.Base.Any.Any lookup_table:Standard.Base.Any.Any lookup_columns:Standard.Base.Any.Any allow_unmatched_rows:Standard.Base.Any.Any ~continuation:Standard.Base.Any.Any -> Standard.Base.Any.Any
- make_context_for_lookup_join lookup_columns:Standard.Base.Any.Any subquery_setup:Standard.Base.Any.Any -> Standard.Base.Any.Any
- make_invariant_check lookup_counter:Standard.Base.Any.Any allow_unmatched_rows:Standard.Base.Any.Any -> Standard.Base.Any.Any
- make_lookup_counter_column connection:Standard.Base.Any.Any lookup_columns:Standard.Base.Any.Any unique_name_strategy:Standard.Base.Any.Any -> Standard.Base.Any.Any
- precheck_for_duplicate_matches lookup_columns:Standard.Base.Any.Any subquery_setup:Standard.Base.Any.Any connection:Standard.Base.Any.Any new_ctx:Standard.Base.Any.Any ~continuation:Standard.Base.Any.Any -> Standard.Base.Any.Any
- prepare_subqueries base_table:Standard.Base.Any.Any lookup_table:Standard.Base.Any.Any lookup_columns:Standard.Base.Any.Any unique_name_strategy:Standard.Base.Any.Any -> Standard.Base.Any.Any
