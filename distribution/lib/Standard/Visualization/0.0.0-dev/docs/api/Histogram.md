## Enso Signatures 1.0
## module Standard.Visualization.Histogram
- type Update
    - Value values:Standard.Base.Any.Any label:Standard.Base.Any.Any
    - to_js_object self -> Standard.Base.Any.Any
- from_table table:Standard.Base.Any.Any -> Standard.Base.Any.Any
- from_value value:Standard.Base.Any.Any -> Standard.Base.Any.Any
- from_vector vector:Standard.Base.Any.Any -> Standard.Base.Any.Any
- process_to_json_text value:Standard.Base.Any.Any -> Standard.Base.Any.Any
- Standard.Table.Table.Table.first_numeric self -> Standard.Base.Any.Any
- Standard.Table.Table.Table.value_column self -> Standard.Base.Any.Any
