## Enso Signatures 1.0
## module Standard.Table.Row
- type Row
    - at self column:(Standard.Base.Data.Numbers.Integer|Standard.Base.Data.Text.Text)= -> Standard.Base.Any.Any
    - column_names self -> Standard.Base.Any.Any
    - get self column:(Standard.Base.Data.Numbers.Integer|Standard.Base.Data.Text.Text)= ~if_missing:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - index self -> Standard.Base.Any.Any
    - length self -> Standard.Base.Any.Any
    - to_dictionary self -> Standard.Base.Any.Any
    - to_js_object self -> Standard.Base.Any.Any
    - to_json_data self -> Standard.Base.Any.Any
    - to_vector self -> Standard.Base.Any.Any
- type Row_Comparator
    - compare obj1:Standard.Base.Any.Any obj2:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - hash obj:Standard.Base.Any.Any -> Standard.Base.Any.Any
- Standard.Base.Data.Vector.Vector.from that:Standard.Table.Row.Row -> Standard.Base.Data.Vector.Vector
- Standard.Base.Data.Ordering.Comparable.from that:Standard.Table.Row.Row -> Standard.Base.Data.Ordering.Comparable
