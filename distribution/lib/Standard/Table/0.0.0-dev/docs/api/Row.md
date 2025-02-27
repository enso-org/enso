## Enso Signatures 1.0
## module Standard.Table.Row
- type Row
    - Value table:Standard.Table.Table.Table index:Standard.Base.Data.Numbers.Integer
    - at self column:(Standard.Base.Data.Numbers.Integer|Standard.Base.Data.Text.Text)= -> Standard.Base.Any.Any
    - column_names self -> Standard.Base.Any.Any
    - get self column:(Standard.Base.Data.Numbers.Integer|Standard.Base.Data.Text.Text)= ~if_missing:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - length self -> Standard.Base.Any.Any
    - to_dictionary self -> Standard.Base.Any.Any
    - to_js_object self -> Standard.Base.Any.Any
    - to_vector self -> Standard.Base.Any.Any
