## Enso Signatures 1.0
## module Standard.Table.Refined_Types.Date_Column
- type Date_Column
    - end_of_month self -> (Standard.Table.Column.Column&Standard.Table.Refined_Types.Date_Column.Date_Column)
    - first_of_month self -> (Standard.Table.Column.Column&Standard.Table.Refined_Types.Date_Column.Date_Column)
    - first_of_week self first_day:Standard.Base.Data.Time.Day_Of_Week.Day_Of_Week= -> (Standard.Table.Column.Column&Standard.Table.Refined_Types.Date_Column.Date_Column)
