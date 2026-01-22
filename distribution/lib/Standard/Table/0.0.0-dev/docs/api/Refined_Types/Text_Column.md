## Enso Signatures 1.0
## module Standard.Table.Refined_Types.Text_Column
- type Text_Column
    - char_length self -> (Standard.Table.Column.Column&Standard.Table.Refined_Types.Numeric_Column.Numeric_Column)
    - index_of self other:(Standard.Table.Column.Column|Standard.Base.Data.Text.Text|Standard.Base.Any.Any) case_sensitivity:Standard.Base.Data.Text.Case_Sensitivity.Case_Sensitivity= -> (Standard.Table.Column.Column&Standard.Table.Refined_Types.Numeric_Column.Numeric_Column)
    - left self n:(Standard.Table.Column.Column|Standard.Base.Data.Numbers.Integer|Standard.Base.Nothing.Nothing)= -> (Standard.Table.Column.Column&Standard.Table.Refined_Types.Text_Column.Text_Column)
    - length self -> (Standard.Table.Column.Column&Standard.Table.Refined_Types.Numeric_Column.Numeric_Column)
    - lower self -> (Standard.Table.Column.Column&Standard.Table.Refined_Types.Text_Column.Text_Column)
    - mid self start:(Standard.Table.Column.Column|Standard.Base.Any.Any) length:(Standard.Table.Column.Column|Standard.Base.Any.Any|Standard.Table.Column.Rest_Of_String)= -> (Standard.Table.Column.Column&Standard.Table.Refined_Types.Text_Column.Text_Column)
    - proper self -> (Standard.Table.Column.Column&Standard.Table.Refined_Types.Text_Column.Text_Column)
    - right self n:(Standard.Table.Column.Column|Standard.Base.Data.Numbers.Integer|Standard.Base.Nothing.Nothing)= -> (Standard.Table.Column.Column&Standard.Table.Refined_Types.Text_Column.Text_Column)
    - to_case self case_option:Standard.Base.Data.Text.Case.Case= locale:Standard.Base.Data.Locale.Locale= -> (Standard.Table.Column.Column&Standard.Table.Refined_Types.Text_Column.Text_Column)
    - upper self -> (Standard.Table.Column.Column&Standard.Table.Refined_Types.Text_Column.Text_Column)
