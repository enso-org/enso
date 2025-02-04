## Enso Signatures 1.0
## module Standard.Base.Panic
- type Caught_Panic
    - Panic payload:Standard.Base.Any.Any internal_original_exception:Standard.Base.Any.Any
    - convert_to_dataflow_error self -> Standard.Base.Any.Any
    - stack_trace self -> Standard.Base.Any.Any
- type Panic
    - catch panic_type:Standard.Base.Any.Any ~action:Standard.Base.Any.Any handler:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - get_attached_stack_trace error:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - handle_wrapped_dataflow_error ~action:Standard.Base.Any.Any error_type:Standard.Base.Any.Any= handler:(Standard.Base.Any.Any|Standard.Base.Nothing.Nothing)= -> Standard.Base.Any.Any
    - primitive_get_attached_stack_trace throwable:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - recover expected_types:Standard.Base.Any.Any ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - rethrow value:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - rethrow_wrapped_if_error value:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - throw payload:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - with_finalizer ~finalizer:Standard.Base.Any.Any ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
- type Wrapped_Dataflow_Error
    - Error payload:Standard.Base.Any.Any
    - to_display_text self -> Standard.Base.Data.Text.Text
    - unwrap self -> Standard.Base.Any.Any
