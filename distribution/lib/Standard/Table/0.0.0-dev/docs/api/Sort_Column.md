## Enso Signatures 1.0
## module Standard.Table.Sort_Column
- type Sort_Column
    - Index index:Standard.Base.Data.Numbers.Integer= direction:Standard.Base.Data.Sort_Direction.Sort_Direction=
    - Name name:Standard.Base.Data.Text.Text= direction:Standard.Base.Data.Sort_Direction.Sort_Direction=
    - Select_By_Name name:(Standard.Base.Data.Text.Text|Standard.Base.Data.Text.Regex.Regex)= direction:Standard.Base.Data.Sort_Direction.Sort_Direction= case_sensitivity:Standard.Base.Data.Text.Case_Sensitivity.Case_Sensitivity=
- Standard.Table.Sort_Column.Sort_Column.from that:Standard.Base.Data.Text.Text -> Standard.Table.Sort_Column.Sort_Column
- Standard.Table.Sort_Column.Sort_Column.from that:Standard.Base.Data.Numbers.Integer -> Standard.Table.Sort_Column.Sort_Column
