## Enso Signatures 1.0
## module Standard.Table.Spatial_Table
- type Spatial_Join_Condition
    - Contains left:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)= right:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)=
    - Contains_Properly left:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)= right:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)=
    - Covered_By left:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)= right:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)=
    - Covers left:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)= right:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)=
    - Crosses left:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)= right:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)=
    - Intersect left:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)= right:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)=
    - Overlaps left:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)= right:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)=
    - Touches left:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)= right:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)=
    - Within left:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)= right:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)=
- type Spatial_Table
- Standard.Table.Spatial_Table.Spatial_Table.from that:Standard.Table.Table.Table -> Standard.Table.Spatial_Table.Spatial_Table
- Standard.Table.Table.Table.from that:Standard.Table.Spatial_Table.Spatial_Table -> Standard.Table.Table.Table
- Standard.Table.Join_Condition.Join_Condition.from that:Standard.Table.Spatial_Table.Spatial_Join_Condition -> Standard.Table.Join_Condition.Join_Condition
