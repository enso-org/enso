## Enso Signatures 1.0
## module Standard.Base.Error
- type Error
    - catch self error_type:Standard.Base.Any.Any= handler:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - get_stack_trace_text self -> Standard.Base.Any.Any
    - if_not_error self ~other:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - is_error self -> Standard.Base.Any.Any
    - map_error self f:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - stack_trace self -> Standard.Base.Any.Any
    - throw payload:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - to_display_text self -> Standard.Base.Any.Any
    - to_text self -> Standard.Base.Any.Any
    - unwrap error_value:Standard.Base.Any.Any -> Standard.Base.Any.Any
- look_for_wrapped_error error_type:Standard.Base.Any.Any= error_value:Standard.Base.Any.Any -> Standard.Base.Any.Any
