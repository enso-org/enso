## Enso Signatures 1.0
## module Standard.Base.System.Process.Process_Builder
- type Process_Builder
    - Value command:Standard.Base.Any.Any arguments:Standard.Base.Any.Any stdin:Standard.Base.Any.Any
    - create self -> Standard.Base.Any.Any
    - set_arguments self arguments:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - set_stdin self stdin:Standard.Base.Any.Any -> Standard.Base.Any.Any
- type Process_Result
    - Value exit_code:Standard.Base.System.Process.Exit_Code.Exit_Code stdout:Standard.Base.Data.Text.Text stderr:Standard.Base.Data.Text.Text
    - to_display_text self -> Standard.Base.Any.Any
