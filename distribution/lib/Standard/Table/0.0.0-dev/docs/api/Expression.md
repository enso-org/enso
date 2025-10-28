## Enso Signatures 1.0
## module Standard.Table.Expression
- type Expression
    - Value expression:Standard.Base.Data.Text.Text
    - evaluate expression:Standard.Table.Expression.Expression table:Standard.Table.Table.Table -> Standard.Base.Any.Any
- type Expression_Error
    - Argument_Mismatch message:Standard.Base.Data.Text.Text
    - Syntax_Error message:Standard.Base.Data.Text.Text line:Standard.Base.Data.Numbers.Integer column:Standard.Base.Data.Numbers.Integer
    - Type_Error message:Standard.Base.Data.Text.Text
    - Unsupported_Operation name:Standard.Base.Data.Text.Text
    - to_display_text self -> Standard.Base.Any.Any
- expr expression:Standard.Base.Data.Text.Text -> Standard.Table.Expression.Expression
- handle_java_error java_type:Standard.Base.Any.Any enso_constructor:Standard.Base.Any.Any -> Standard.Base.Any.Any
