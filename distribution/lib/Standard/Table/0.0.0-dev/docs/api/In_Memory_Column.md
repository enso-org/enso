## Enso Signatures 1.0
## module Standard.Table.In_Memory_Column
- type In_Memory_Column
    - await_metrics self -> Standard.Base.Data.Dictionary.Dictionary
    - distinct_values_json self -> (Standard.Base.Data.Text.Text|Standard.Base.Nothing.Nothing)
    - max_precision self -> Standard.Base.Any.Any
    - pretty self -> Standard.Base.Any.Any
    - to_json_data self start:Standard.Base.Data.Numbers.Integer= row_count:Standard.Base.Data.Numbers.Integer= -> Standard.Base.Data.Text.Text
    - to_table_viz_json self all_row_count:Standard.Base.Data.Numbers.Integer= use_server_mode:Standard.Base.Data.Boolean.Boolean= -> Standard.Base.Data.Text.Text
    - to_text self -> Standard.Base.Data.Text.Text
- Standard.Table.Column.Column.from that:Standard.Table.In_Memory_Column.In_Memory_Column -> Standard.Table.Column.Column
