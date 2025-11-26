## Enso Signatures 1.0
## module Standard.Microsoft.Connection.SQLServer_Query
- type SQLServer_Query
    - Raw_SQL sql:Standard.Base.Data.Text.Text=
    - Table_Name name:Standard.Base.Data.Text.Text= schema:Standard.Base.Data.Text.Text=
    - to_db_table self connection:Standard.Base.Any.Any alias:Standard.Base.Data.Text.Text -> Standard.Base.Any.Any
- Standard.Microsoft.Connection.SQLServer_Query.SQLServer_Query.from that:Standard.Base.Data.Text.Text -> Standard.Microsoft.Connection.SQLServer_Query.SQLServer_Query
- Standard.Microsoft.Connection.SQLServer_Query.SQLServer_Query.from that:Standard.Database.SQL_Query.SQL_Query -> Standard.Microsoft.Connection.SQLServer_Query.SQLServer_Query
