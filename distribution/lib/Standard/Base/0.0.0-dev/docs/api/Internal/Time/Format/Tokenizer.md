## Enso Signatures 1.0
## module Standard.Base.Internal.Time.Format.Tokenizer
- type Format_Token
    - Curly_Section inner_text:Standard.Base.Data.Text.Text
    - Literal text:Standard.Base.Data.Text.Text
    - Optional_Section_End
    - Optional_Section_Start
    - Pattern character:Standard.Base.Data.Text.Text count:Standard.Base.Data.Numbers.Integer
- type Tokenizer
    - Instance original_text:Standard.Base.Data.Text.Text chars:(Standard.Base.Data.Vector.Vector Standard.Base.Data.Text.Text) tokens_builder:(Standard.Base.Data.Vector.Builder Standard.Base.Internal.Time.Format.Tokenizer.Format_Token) optional_nesting:(Standard.Base.Runtime.Ref.Ref Standard.Base.Data.Numbers.Integer)
    - enter_optional_section self -> Standard.Base.Any.Any
    - exit_optional_section self -> Standard.Base.Any.Any
    - finalize_token self current_token:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - is_in_optional self -> Standard.Base.Any.Any
    - new builder:Standard.Base.Any.Any text:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - parse_curly self position:Standard.Base.Any.Any text_accumulator:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - parse_normal self position:Standard.Base.Any.Any current_token:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - parse_quoted self position:Standard.Base.Any.Any text_accumulator:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - tokenize text:Standard.Base.Any.Any -> Standard.Base.Any.Any
