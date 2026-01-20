## Enso Signatures 1.0
## module Standard.Table.Internal.Problem_Builder
- type Problem_Builder
    - attach_problems_after self problem_behavior:Standard.Base.Any.Any result:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - attach_problems_before self problem_behavior:Standard.Base.Errors.Problem_Behavior.Problem_Behavior ~computation:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - build_problemset self -> Standard.Base.Any.Any
    - get_problemset_throwing_distinguished_errors self -> Standard.Base.Any.Any
    - new types_to_always_throw:Standard.Base.Any.Any= error_on_missing_columns:Standard.Base.Any.Any= missing_input_columns_location:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - raise_no_output_columns_with_cause self -> Standard.Base.Any.Any
    - report_missing_input_columns self columns:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - report_oob_indices self indices:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - report_other_warning self warning:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - report_unique_name_strategy self unique_name_strategy:Standard.Base.Any.Any -> Standard.Base.Any.Any
- append_to_ref ref:Standard.Base.Any.Any vector:Standard.Base.Any.Any -> Standard.Base.Any.Any
