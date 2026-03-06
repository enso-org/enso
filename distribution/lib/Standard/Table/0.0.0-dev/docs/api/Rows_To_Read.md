## Enso Signatures 1.0
## module Standard.Table.Rows_To_Read
- type Rows_To_Read
    - All_Rows
    - First rows:Standard.Base.Data.Numbers.Integer= when:Standard.Table.Rows_To_Read.When=
    - First_With_Warning rows:Standard.Base.Data.Numbers.Integer=
    - attach_warning self input:Standard.Table.Table.Table -> Standard.Table.Table.Table
    - attach_warning_vector self input:Standard.Base.Data.Vector.Vector -> Standard.Base.Data.Vector.Vector
    - default_widget display:Standard.Base.Metadata.Display= -> Standard.Base.Metadata.Widget
    - rows_to_read self -> (Standard.Base.Data.Numbers.Integer|Standard.Base.Nothing.Nothing)
    - rows_to_write self -> (Standard.Base.Data.Numbers.Integer|Standard.Base.Nothing.Nothing)
- Standard.Table.Rows_To_Read.Rows_To_Read.from that:Standard.Base.Nothing.Nothing -> Standard.Table.Rows_To_Read.Rows_To_Read
- Standard.Table.Rows_To_Read.Rows_To_Read.from that:Standard.Base.Data.Numbers.Integer -> Standard.Table.Rows_To_Read.Rows_To_Read
- type When
    - Always
    - In_Design_Mode
