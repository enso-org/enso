## Enso Signatures 1.0
## module Standard.Table.Return_As_Table
- type Return_As_Table
    - As_Merged_Table columns_to_keep:Standard.Table.Columns_To_Keep.Columns_To_Keep= match:Standard.Table.Match_Columns.Match_Columns=
    - With_New_Column
    - get_dropdown_options -> Standard.Base.Any.Any
    - make_return self input:Standard.Base.Data.Read.Many_Files_List.Many_Files_List objects:(Standard.Base.Data.Vector.Vector Standard.Base.Any.Any) on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Base.Any.Any
    - resolve value:Standard.Base.Any.Any -> Standard.Base.Any.Any
- Standard.Base.Data.Read.Return_As.Return_As.from that:Standard.Table.Return_As_Table.Return_As_Table -> Standard.Base.Data.Read.Return_As.Return_As
