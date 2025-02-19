## Enso Signatures 1.0
## module Standard.Base.System.Output_Stream
- type Output_Stream
    - close self -> Standard.Base.Any.Any
    - new java_stream:Standard.Base.Any.Any error_handler:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - with_java_stream self f:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - with_memory_stream action:Standard.Base.Any.Any -> (Standard.Base.Data.Pair.Pair Standard.Base.Data.Vector.Vector Standard.Base.Any.Any)
    - with_stream_encoder self encoding:Standard.Base.Data.Text.Encoding.Encoding on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior action:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - write_bytes self contents:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - write_stream self input_stream:Standard.Base.System.Input_Stream.Input_Stream -> Standard.Base.Any.Any
