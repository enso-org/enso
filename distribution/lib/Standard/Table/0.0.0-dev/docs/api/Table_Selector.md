## Enso Signatures 1.0
## module Standard.Table.Table_Selector
- type Table_Selector
    - By_Type by_type:Standard.Table.Value_Type.By_Type
    - Index index:Standard.Base.Data.Numbers.Integer
    - Name name:Standard.Base.Data.Text.Text
    - Regex pattern:Standard.Base.Data.Text.Regex.Regex
    - Vector selectors:(Standard.Base.Data.Vector.Vector Standard.Table.Table_Selector.Table_Selector)
    - act_if_any self table:Standard.Table.Table.Table error_on_missing_columns:Standard.Base.Data.Boolean.Boolean action:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - resolve self table:Standard.Table.Table.Table error_on_missing_columns:Standard.Base.Data.Boolean.Boolean -> Standard.Base.Any.Any
- Standard.Table.Table_Selector.Table_Selector.from that:Standard.Base.Data.Text.Text -> Standard.Table.Table_Selector.Table_Selector
- Standard.Table.Table_Selector.Table_Selector.from that:Standard.Base.Data.Numbers.Integer -> Standard.Table.Table_Selector.Table_Selector
- Standard.Table.Table_Selector.Table_Selector.from that:Standard.Base.Data.Text.Regex.Regex -> Standard.Table.Table_Selector.Table_Selector
- Standard.Table.Table_Selector.Table_Selector.from that:Standard.Table.Value_Type.By_Type -> Standard.Table.Table_Selector.Table_Selector
- Standard.Table.Table_Selector.Table_Selector.from that:Standard.Base.Function.Function -> Standard.Table.Table_Selector.Table_Selector
- Standard.Table.Table_Selector.Table_Selector.from that:Standard.Base.Data.Vector.Vector -> Standard.Table.Table_Selector.Table_Selector
