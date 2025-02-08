## Enso Signatures 1.0
## module Standard.Base.System.Input_Stream
- type Input_Stream
    - as_peekable_stream self -> Standard.Base.System.Input_Stream.Input_Stream
    - as_restartable_stream self extend_lifetime:Standard.Base.Data.Boolean.Boolean= -> Standard.Base.System.Advanced.Restartable_Input_Stream.Restartable_Input_Stream
    - close self -> Standard.Base.Any.Any
    - from_bytes bytes:Standard.Base.Any.Any -> Standard.Base.System.Input_Stream.Input_Stream
    - is_peekable self -> Standard.Base.Data.Boolean.Boolean
    - new java_stream:Standard.Base.Any.Any error_handler:Standard.Base.Any.Any associated_source:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - peek_bytes self n:Standard.Base.Data.Numbers.Integer -> (Standard.Base.Data.Vector.Vector Standard.Base.Data.Numbers.Integer)
    - read_all_bytes self -> Standard.Base.Any.Any
    - read_byte self -> Standard.Base.Any.Any
    - read_n_bytes self n:Standard.Base.Data.Numbers.Integer -> Standard.Base.Any.Any
    - skip_n_bytes self n:Standard.Base.Data.Numbers.Integer -> Standard.Base.Any.Any
    - with_java_stream self f:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - with_stream_decoder self encoding:Standard.Base.Data.Text.Encoding.Encoding on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior action:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - write_to_file self file:Standard.Base.System.File.Generic.Writable_File.Writable_File -> Standard.Base.Any.Any
- close_stream stream:Standard.Base.Any.Any -> Standard.Base.Any.Any
