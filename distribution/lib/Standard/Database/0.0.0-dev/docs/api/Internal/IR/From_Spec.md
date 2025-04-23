## Enso Signatures 1.0
## module Standard.Database.Internal.IR.SQL_IR_From_Part
- type SQL_IR_From_Part
    - Join kind:Standard.Database.Internal.IR.SQL_Join_Kind.SQL_Join_Kind left_spec:Standard.Database.Internal.IR.SQL_IR_From_Part.SQL_IR_From_Part right_spec:Standard.Database.Internal.IR.SQL_IR_From_Part.SQL_IR_From_Part on:(Standard.Base.Data.Vector.Vector Standard.Database.Internal.IR.SQL_IR_Expression.SQL_IR_Expression)
    - Literal_Values column_vectors:(Standard.Base.Data.Vector.Vector Standard.Base.Any.Any) column_names:(Standard.Base.Data.Vector.Vector Standard.Base.Data.Text.Text) alias:Standard.Base.Data.Text.Text
    - Query raw_sql:(Standard.Base.Data.Text.Text|Standard.Database.SQL_Statement.SQL_Statement) alias:Standard.Base.Data.Text.Text
    - Sub_Query columns:(Standard.Base.Data.Vector.Vector Standard.Base.Any.Any) context:Standard.Database.Internal.IR.SQL_IR_Source.SQL_IR_Source alias:Standard.Base.Data.Text.Text
    - Table table_name:Standard.Base.Data.Text.Text alias:Standard.Base.Data.Text.Text internal_temporary_keep_alive_reference:Standard.Base.Any.Any=
    - Union queries:(Standard.Base.Data.Vector.Vector Standard.Database.Internal.IR.Query.Query) alias:Standard.Base.Data.Text.Text
    - traverse self f:Standard.Base.Any.Any -> Standard.Database.Internal.IR.SQL_IR_From_Part.SQL_IR_From_Part
- type SQL_IR_From_Part_Comparator
    - compare x:Standard.Base.Any.Any y:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - hash x:Standard.Base.Any.Any -> Standard.Base.Any.Any
- Standard.Base.Data.Ordering.Comparable.from that:Standard.Database.Internal.IR.SQL_IR_From_Part.SQL_IR_From_Part -> Standard.Base.Data.Ordering.Comparable
