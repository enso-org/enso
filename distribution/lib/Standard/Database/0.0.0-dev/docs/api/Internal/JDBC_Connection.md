## Enso Signatures 1.0
## module Standard.Database.Internal.JDBC_Connection
- type JDBC_Connection
    - batch_insert self insert_template:Standard.Base.Any.Any statement_setter:Standard.Base.Any.Any table:Standard.Base.Any.Any batch_size:Standard.Base.Any.Any expected_type_hints:Standard.Base.Any.Any= row_limit:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - close self -> Standard.Base.Any.Any
    - ensure_query_has_no_holes self raw_sql:Standard.Base.Data.Text.Text -> Standard.Base.Any.Any
    - run_maintenance_action_if_possible self callback:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - run_within_transaction self ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - with_connection self action:Standard.Base.Any.Any related_query:(Standard.Base.Nothing.Nothing|Standard.Base.Data.Text.Text)= -> Standard.Base.Any.Any
    - with_metadata self ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - with_prepared_statement self query:(Standard.Base.Data.Text.Text|Standard.Database.SQL.SQL_Statement) statement_setter:Standard.Base.Any.Any action:Standard.Base.Any.Any skip_log:Standard.Base.Any.Any= -> Standard.Base.Any.Any
- create types_record:Standard.Base.Any.Any url:Standard.Base.Data.Text.Text properties:Standard.Base.Data.Vector.Vector catalog:Standard.Base.Data.Text.Text= schema:Standard.Base.Data.Text.Text= -> Standard.Base.Any.Any
