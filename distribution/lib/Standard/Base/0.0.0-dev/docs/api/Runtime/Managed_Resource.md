## Enso Signatures 1.0
## module Standard.Base.Runtime.Managed_Resource
- type Managed_Resource
    - bracket ~constructor:Standard.Base.Any.Any ~destructor:Standard.Base.Any.Any ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - finalize self -> Standard.Base.Any.Any
    - has_been_finalized self -> Standard.Base.Data.Boolean.Boolean
    - register resource:Standard.Base.Any.Any function:Standard.Base.Any.Any system_finalization_allowed:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - take self -> Standard.Base.Any.Any
    - with self ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
- register_builtin r:Standard.Base.Any.Any fn:Standard.Base.Any.Any sys:Standard.Base.Data.Boolean.Boolean -> Standard.Base.Any.Any
- with_builtin r:Standard.Base.Any.Any fn:Standard.Base.Any.Any -> Standard.Base.Any.Any
