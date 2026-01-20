## Enso Signatures 1.0
## module Standard.Table.Grouping_Method
- type Group_Row
    - First
    - Previous
- type Grouping_Method
    - Equal_Count group_count:Standard.Base.Data.Numbers.Integer= order_by:(Standard.Base.Data.Vector.Vector|Standard.Base.Data.Text.Text)=
    - Equal_Sum on:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)= group_count:Standard.Base.Data.Numbers.Integer= order_by:(Standard.Base.Data.Vector.Vector|Standard.Base.Data.Text.Text)=
    - Standard_Deviation on:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)= population:Standard.Base.Data.Boolean.Boolean=
    - Unique on:(Standard.Base.Data.Vector.Vector|Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer|Standard.Base.Data.Text.Regex.Regex)=
- type Predicate_Over
    - Index index:Standard.Base.Data.Numbers.Integer
    - Name column:Standard.Base.Data.Text.Text
    - Row
- Standard.Table.Grouping_Method.Predicate_Over.from that:Standard.Base.Data.Text.Text -> Standard.Table.Grouping_Method.Predicate_Over
- Standard.Table.Grouping_Method.Predicate_Over.from that:Standard.Base.Data.Numbers.Integer -> Standard.Table.Grouping_Method.Predicate_Over
