## Enso Signatures 1.0
## module Standard.Database.DB_Column
- type DB_Column
    - dialect_name self -> Standard.Base.Data.Text.Text
    - let self name:Standard.Base.Data.Text.Text callback:Standard.Base.Any.Any -> (Standard.Table.Column.Column&Standard.Database.DB_Column.DB_Column)
    - pretty self -> Standard.Base.Any.Any
    - read self max_rows:Standard.Table.Rows_To_Read.Rows_To_Read= -> Standard.Table.Column.Column
    - to_sql self -> Standard.Database.SQL_Statement.SQL_Statement
    - to_text self -> Standard.Base.Data.Text.Text
- Standard.Table.Column.Column.from that:Standard.Database.DB_Column.DB_Column -> Standard.Table.Column.Column
- Standard.Database.DB_Column.DB_Column.from that:Standard.Table.Column.Column -> Standard.Database.DB_Column.DB_Column
- Standard.Table.Refined_Types.Numeric_Column.Numeric_Column.from that:Standard.Database.DB_Column.DB_Column -> Standard.Table.Refined_Types.Numeric_Column.Numeric_Column
