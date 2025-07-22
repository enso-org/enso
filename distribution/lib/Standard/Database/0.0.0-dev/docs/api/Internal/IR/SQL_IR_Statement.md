## Enso Signatures 1.0
## module Standard.Database.Internal.IR.SQL_IR_Statement
- type SQL_IR_Statement
    - Create_Table table_name:Standard.Base.Data.Text.Text columns:(Standard.Base.Data.Vector.Vector Standard.Database.Internal.IR.Create_Column_Descriptor.Create_Column_Descriptor) primary_key:(Standard.Base.Nothing.Nothing|Standard.Base.Any.Any) temporary:Standard.Base.Data.Boolean.Boolean
    - Delete_Matching_Rows target_table_name:Standard.Base.Data.Text.Text source_table_name:Standard.Base.Data.Text.Text key_columns:(Standard.Base.Data.Vector.Vector Standard.Base.Data.Text.Text)
    - Delete_Unmatched_Rows target_table_name:Standard.Base.Data.Text.Text source_table_name:Standard.Base.Data.Text.Text key_columns:(Standard.Base.Data.Vector.Vector Standard.Base.Data.Text.Text)
    - Drop_Table table_name:Standard.Base.Data.Text.Text if_exists:Standard.Base.Data.Boolean.Boolean
    - Insert table_name:Standard.Base.Data.Text.Text pairs:(Standard.Base.Data.Vector.Vector Standard.Base.Any.Any)
    - Insert_From_Select table_name:Standard.Base.Data.Text.Text column_names:(Standard.Base.Data.Vector.Vector Standard.Base.Data.Text.Text) select:Standard.Database.Internal.IR.SQL_IR_Statement.SQL_IR_Statement
    - Select expressions:(Standard.Base.Nothing.Nothing|Standard.Base.Any.Any) context:Standard.Database.Internal.IR.SQL_IR_Source.SQL_IR_Source
    - Truncate_Table table_name:Standard.Base.Data.Text.Text
    - Update_From_Table target_table_name:Standard.Base.Data.Text.Text source_table_name:Standard.Base.Data.Text.Text column_names:(Standard.Base.Data.Vector.Vector Standard.Base.Data.Text.Text) key_columns:(Standard.Base.Data.Vector.Vector Standard.Base.Data.Text.Text)
    - traverse self f:Standard.Base.Any.Any -> Standard.Database.Internal.IR.SQL_IR_Statement.SQL_IR_Statement
