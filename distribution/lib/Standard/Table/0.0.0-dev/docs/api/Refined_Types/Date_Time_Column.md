## Enso Signatures 1.0
## module Standard.Table.Refined_Types.Date_Time_Column
- type Date_Time_Column
    - at_time_zone self time_zone:(Standard.Base.Data.Time.Time_Zone.Time_Zone|Standard.Base.Data.Text.Text) -> (Standard.Table.Column.Column&Standard.Table.Refined_Types.Date_Time_Column.Date_Time_Column)
    - set_time_zone self time_zone:(Standard.Base.Data.Time.Time_Zone.Time_Zone|Standard.Base.Data.Text.Text) -> (Standard.Table.Column.Column&Standard.Table.Refined_Types.Date_Time_Column.Date_Time_Column)
