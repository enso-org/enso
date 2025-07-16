## Enso Signatures 1.0
## module Standard.Table.In_Memory_Table
- type In_Memory_Table
    - new columns:(Standard.Base.Data.Vector.Vector Standard.Base.Any.Any) -> (Standard.Table.Table.Table|Standard.Base.Any.Any)
    - to_delimited self delimiter:(Standard.Table.Delimited.Delimited_Format.Delimited_Format|Standard.Base.Data.Text.Text)= quote_style:Standard.Table.Delimited.Quote_Style.Quote_Style= headers:Standard.Table.Headers.Headers= value_formatter:(Standard.Table.Data_Formatter.Data_Formatter|Standard.Base.Nothing.Nothing)= line_endings:(Standard.Base.Data.Text.Line_Ending_Style.Line_Ending_Style|Standard.Base.System.File_Format.Infer)= -> Standard.Base.Data.Text.Text
- from_java_table java_table:Standard.Base.Any.Any -> Standard.Base.Any.Any
- to_java_table table:Standard.Table.In_Memory_Table.In_Memory_Table -> Standard.Base.Any.Any
- Standard.Table.Table.Table.from that:Standard.Table.In_Memory_Table.In_Memory_Table -> Standard.Table.Table.Table
