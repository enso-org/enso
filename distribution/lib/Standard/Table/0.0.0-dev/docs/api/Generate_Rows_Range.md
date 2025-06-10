## Enso Signatures 1.0
## module Standard.Table.Generate_Rows_Range
- type Generate_Rows_Range
    - Date_Range from:(Standard.Base.Data.Time.Date.Date|Standard.Table.Column_Ref.Column_Ref|Standard.Table.Expression.Expression)= to:(Standard.Base.Data.Time.Date.Date|Standard.Table.Column_Ref.Column_Ref|Standard.Table.Expression.Expression)= include_end:Standard.Base.Data.Boolean.Boolean= step:(Standard.Base.Data.Time.Date_Period.Date_Period|Standard.Base.Data.Time.Period.Period)=
    - Integer_Column value:Standard.Table.Column.Column
    - Integer_Range from:(Standard.Base.Data.Numbers.Integer|Standard.Table.Column_Ref.Column_Ref|Standard.Table.Expression.Expression)= to:(Standard.Base.Data.Numbers.Integer|Standard.Table.Column_Ref.Column_Ref|Standard.Table.Expression.Expression)= include_end:Standard.Base.Data.Boolean.Boolean= step:Standard.Base.Data.Numbers.Integer=
    - Range value:Standard.Base.Any.Any
    - default_widget table:Standard.Table.Internal.Table_Ref.Table_Ref display:Standard.Base.Metadata.Display= -> Standard.Base.Metadata.Widget
    - get_range_column self table:Standard.Base.Any.Any on_problems:Standard.Base.Any.Any -> Standard.Table.Column.Column
- Standard.Table.Generate_Rows_Range.Generate_Rows_Range.from that:Standard.Base.Data.Numbers.Integer -> Standard.Table.Generate_Rows_Range.Generate_Rows_Range
- Standard.Table.Generate_Rows_Range.Generate_Rows_Range.from that:Standard.Base.Data.Range.Range -> Standard.Table.Generate_Rows_Range.Generate_Rows_Range
- Standard.Table.Generate_Rows_Range.Generate_Rows_Range.from that:Standard.Base.Data.Time.Date_Range.Date_Range -> Standard.Table.Generate_Rows_Range.Generate_Rows_Range
- Standard.Table.Generate_Rows_Range.Generate_Rows_Range.from that:Standard.Table.Column.Column -> Standard.Table.Generate_Rows_Range.Generate_Rows_Range
