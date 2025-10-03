## Enso Signatures 1.0
## module Standard.Table.In_Memory_Column
- type In_Memory_Column
    - max_precision self -> Standard.Base.Any.Any
    - pretty self -> Standard.Base.Any.Any
    - to_json_data self start:Standard.Base.Data.Numbers.Integer= row_count:Standard.Base.Data.Numbers.Integer= fallback:Standard.Base.Any.Any= -> Standard.Base.Data.Text.Text
    - to_text self -> Standard.Base.Data.Text.Text
- Standard.Table.Column.Column.from that:Standard.Table.In_Memory_Column.In_Memory_Column -> Standard.Table.Column.Column
- Standard.Table.Internal.Visualization_Helpers.Visualization_Helpers.from that:Standard.Table.In_Memory_Column.In_Memory_Column -> Standard.Table.Internal.Visualization_Helpers.Visualization_Helpers
