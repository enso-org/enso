## Enso Signatures 1.0
## module Standard.Database.Internal.Internals_Access
- column_expression column:(Standard.Database.DB_Column.DB_Column|Standard.Database.Internal.IR.Internal_Column.Internal_Column) -> Standard.Database.Internal.IR.SQL_IR_Expression.SQL_IR_Expression
- get_connection thing:(Standard.Database.DB_Column.DB_Column|Standard.Database.DB_Table.DB_Table) -> Standard.Base.Any.Any
- get_context thing:(Standard.Database.DB_Column.DB_Column|Standard.Database.DB_Table.DB_Table) -> Standard.Database.Internal.IR.SQL_IR_Source.SQL_IR_Source
- internal_columns table:Standard.Database.DB_Table.DB_Table -> (Standard.Base.Data.Vector.Vector Standard.Database.Internal.IR.Internal_Column.Internal_Column)
