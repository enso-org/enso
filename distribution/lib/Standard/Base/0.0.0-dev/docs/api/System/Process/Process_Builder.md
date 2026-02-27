## Enso Signatures 1.0
## module Standard.Base.System.Process.Process_Builder
- type Process_Builder
    - create self redirect_out_err:Standard.Base.Data.Boolean.Boolean= -> Standard.Base.System.Process.Process_Builder.Process_Result
    - set_arguments self arguments:(Standard.Base.Data.Vector.Vector Standard.Base.Data.Text.Text) -> Standard.Base.System.Process.Process_Builder.Process_Builder
    - set_directory self path:Standard.Base.System.File.File -> Standard.Base.System.Process.Process_Builder.Process_Builder
    - set_env self key:Standard.Base.Data.Text.Text value:Standard.Base.Data.Text.Text -> Standard.Base.System.Process.Process_Builder.Process_Builder
    - set_stdin self stdin:Standard.Base.Data.Text.Text -> Standard.Base.System.Process.Process_Builder.Process_Builder
    - to_js_object self -> Standard.Base.Data.Json.JS_Object
- type Process_Result
    - Value exit_code:Standard.Base.System.Process.Exit_Code.Exit_Code stdout:Standard.Base.Data.Text.Text stderr:Standard.Base.Data.Text.Text
    - to_display_text self -> Standard.Base.Data.Text.Text
