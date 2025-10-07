## Enso Signatures 1.0
## module Standard.Database.SQL_Query
- type SQL_Query
    - Raw_SQL sql:Standard.Base.Data.Text.Text=
    - Table_Name name:Standard.Base.Data.Text.Text=
    - to_db_table self connection:Standard.Database.Connection.Connection.Connection alias:Standard.Base.Data.Text.Text -> Standard.Base.Any.Any
- make_table_for_name connection:Standard.Base.Any.Any name:Standard.Base.Any.Any schema:Standard.Base.Any.Any alias:Standard.Base.Any.Any internal_temporary_keep_alive_reference:Standard.Base.Any.Any= -> Standard.Base.Any.Any
- make_table_from_query connection:Standard.Base.Any.Any query:(Standard.Base.Data.Text.Text|Standard.Database.SQL.SQL_Statement) alias:Standard.Base.Data.Text.Text -> (Standard.Table.Table.Table&Standard.Database.DB_Table.DB_Table)
- Standard.Database.SQL_Query.SQL_Query.from that:Standard.Base.Data.Text.Text -> Standard.Database.SQL_Query.SQL_Query
