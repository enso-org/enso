## Enso Signatures 1.0
## module Standard.Base.System
- type System_Process_Result
    - Result exit_code:Standard.Base.Any.Any stdout:Standard.Base.Any.Any stderr:Standard.Base.Any.Any
- create_process command:Standard.Base.Any.Any arguments:Standard.Base.Any.Any input:Standard.Base.Any.Any redirect_in:Standard.Base.Any.Any redirect_out:Standard.Base.Any.Any redirect_err:Standard.Base.Any.Any -> Standard.Base.Any.Any
- default_line_separator -> Standard.Base.Any.Any
- exit code:Standard.Base.Any.Any -> Standard.Base.Any.Any
- nano_time -> Standard.Base.Any.Any
- os -> Standard.Base.Any.Any
