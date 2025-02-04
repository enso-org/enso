## Enso Signatures 1.0
## module Standard.Base.System.Advanced.Restartable_Input_Stream
- type Restartable_Input_Stream
    - make input_stream:Standard.Base.System.Input_Stream.Input_Stream extend_lifetime:Standard.Base.Data.Boolean.Boolean -> Standard.Base.System.Advanced.Restartable_Input_Stream.Restartable_Input_Stream
    - to_text self -> Standard.Base.Data.Text.Text
    - with_fresh_stream self action:Standard.Base.Any.Any -> Standard.Base.Any.Any
- max_in_memory_size -> Standard.Base.Any.Any
