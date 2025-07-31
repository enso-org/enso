## Enso Signatures 1.0
## module Standard.Database.DB_Column
- type DB_Column
    - dialect_name self -> Standard.Base.Data.Text.Text
    - is_database_column object:Standard.Base.Any.Any -> Standard.Base.Data.Boolean.Boolean
    - let self name:Standard.Base.Data.Text.Text callback:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - pretty self -> Standard.Base.Data.Text.Text
    - read self max_rows:Standard.Table.Rows_To_Read.Rows_To_Read= -> (Standard.Table.Column.Column|Standard.Base.Any.Any)
    - to_sql self -> Standard.Database.SQL.SQL_Statement
    - to_text self -> Standard.Base.Data.Text.Text
- Standard.Table.Column.Column.from that:Standard.Database.DB_Column.DB_Column -> Standard.Table.Column.Column
