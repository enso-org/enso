## Enso Signatures 1.0
## module Standard.Table.Refined_Types.Text_Column
- type Text_Column
    - char_length self -> (Standard.Table.Column.Column&Standard.Table.Refined_Types.Numeric_Column.Numeric_Column)
    - length self -> (Standard.Table.Column.Column&Standard.Table.Refined_Types.Numeric_Column.Numeric_Column)
    - lower self -> (Standard.Table.Column.Column&Standard.Table.Refined_Types.Text_Column.Text_Column)
    - proper self -> (Standard.Table.Column.Column&Standard.Table.Refined_Types.Text_Column.Text_Column)
    - to_case self case_option:Standard.Base.Data.Text.Case.Case= locale:Standard.Base.Data.Locale.Locale= -> (Standard.Table.Column.Column&Standard.Table.Refined_Types.Text_Column.Text_Column)
    - upper self -> (Standard.Table.Column.Column&Standard.Table.Refined_Types.Text_Column.Text_Column)
