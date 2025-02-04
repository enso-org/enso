## Enso Signatures 1.0
## module Standard.Base.Errors.Problem_Behavior
- type Problem_Behavior
    - Ignore
    - Report_Error
    - Report_Warning
    - attach_problem_after self decorated_value:Standard.Base.Any.Any problem:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - attach_problem_before self problem:Standard.Base.Any.Any ~decorated_value:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - attach_problems_after self decorated_value:Standard.Base.Any.Any problems:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - attach_problems_before self problems:Standard.Base.Any.Any ~decorated_value:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - escalate_warnings self ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - handle_errors self result:Standard.Base.Any.Any ~fallback:Standard.Base.Any.Any error_type:Standard.Base.Any.Any= -> Standard.Base.Any.Any
