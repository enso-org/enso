## Enso Signatures 1.0
## module Standard.Database.Internal.IR.Internal_Column
- type Internal_Column
    - Value name:Standard.Base.Data.Text.Text sql_type_reference:Standard.Database.Internal.SQL_Type_Reference.SQL_Type_Reference expression:Standard.Database.Internal.IR.SQL_IR_Expression.SQL_IR_Expression
    - rename self new_name:Standard.Base.Any.Any -> Standard.Base.Any.Any
- Standard.Database.Internal.IR.Internal_Column.Internal_Column.from that:Standard.Database.DB_Column.DB_Column -> Standard.Database.Internal.IR.Internal_Column.Internal_Column
- Standard.Database.Internal.IR.Internal_Column.Internal_Column.from that:Standard.Table.Column.Column -> Standard.Database.Internal.IR.Internal_Column.Internal_Column
