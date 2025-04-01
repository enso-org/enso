## Enso Signatures 1.0
## module Standard.Database.JDBC.Generic_JDBC_Connection
- type Generic_JDBC_Connection
    - close self -> Standard.Base.Nothing.Nothing
    - connect url:Standard.Base.Data.Text.Text= properties:Standard.Base.Data.Vector.Vector= -> Standard.Database.JDBC.Generic_JDBC_Connection.Generic_JDBC_Connection
    - execute self sql:Standard.Base.Data.Text.Text= -> Standard.Base.Data.Numbers.Integer
    - get_catalogs self -> Standard.Base.Data.Vector.Vector
    - get_schemas self catalog:(Standard.Base.Data.Text.Text|Standard.Base.Nothing.Nothing)= schema_pattern:(Standard.Base.Data.Text.Text|Standard.Base.Nothing.Nothing)= -> Standard.Base.Data.Vector.Vector
    - get_table_info self catalog:(Standard.Base.Data.Text.Text|Standard.Base.Nothing.Nothing)= schema_pattern:(Standard.Base.Data.Text.Text|Standard.Base.Nothing.Nothing)= table_name_pattern:(Standard.Base.Data.Text.Text|Standard.Base.Nothing.Nothing)= table_types:(Standard.Base.Data.Text.Text|Standard.Base.Nothing.Nothing)= -> Standard.Table.Table.Table
    - get_tables self catalog:(Standard.Base.Data.Text.Text|Standard.Base.Nothing.Nothing)= schema_pattern:(Standard.Base.Data.Text.Text|Standard.Base.Nothing.Nothing)= table_name_pattern:(Standard.Base.Data.Text.Text|Standard.Base.Nothing.Nothing)= table_types:(Standard.Base.Data.Vector.Vector|Standard.Base.Nothing.Nothing)= -> Standard.Base.Data.Vector.Vector
    - quote_identifier self identifier:Standard.Base.Data.Text.Text -> Standard.Base.Data.Text.Text
    - quote_literal self literal:Standard.Base.Data.Text.Text -> Standard.Base.Data.Text.Text
    - read self sql_query:Standard.Database.SQL_Query.SQL_Query= -> Standard.Table.Table.Table
    - with_metadata self f:Standard.Base.Any.Any -> Standard.Base.Any.Any
- type Generic_JDBC_Details
    - Value url:Standard.Base.Data.Text.Text
    - connect self options:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - resolve constructor:Standard.Base.Any.Any -> Standard.Base.Any.Any
