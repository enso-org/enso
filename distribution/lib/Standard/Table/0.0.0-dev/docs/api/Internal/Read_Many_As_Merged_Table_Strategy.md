## Enso Signatures 1.0
## module Standard.Table.Internal.Read_Many_As_Merged_Table_Strategy
- type Read_Many_As_Merged_Table_Strategy
    - Value into_table:(Standard.Base.Any.Any -> Standard.Table.Columns_To_Keep.Columns_To_Keep -> Standard.Table.Match_Columns.Match_Columns -> Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Table.Internal.Read_Many_As_Merged_Table_Strategy.Read_Many_As_Table_Result)
- Standard.Table.Internal.Read_Many_As_Merged_Table_Strategy.Read_Many_As_Merged_Table_Strategy.from that:Standard.Table.Table.Table -> Standard.Table.Internal.Read_Many_As_Merged_Table_Strategy.Read_Many_As_Merged_Table_Strategy
- Standard.Table.Internal.Read_Many_As_Merged_Table_Strategy.Read_Many_As_Merged_Table_Strategy.from that:Standard.Base.Data.Vector.Vector -> Standard.Table.Internal.Read_Many_As_Merged_Table_Strategy.Read_Many_As_Merged_Table_Strategy
- Standard.Table.Internal.Read_Many_As_Merged_Table_Strategy.Read_Many_As_Merged_Table_Strategy.from that:Standard.Base.Errors.Common.Failed_To_Load -> Standard.Table.Internal.Read_Many_As_Merged_Table_Strategy.Read_Many_As_Merged_Table_Strategy
- Standard.Table.Internal.Read_Many_As_Merged_Table_Strategy.Read_Many_As_Merged_Table_Strategy.from that:Standard.Base.Any.Any -> Standard.Table.Internal.Read_Many_As_Merged_Table_Strategy.Read_Many_As_Merged_Table_Strategy
- type Read_Many_As_Table_Result
    - No_Data
    - Table metadata:(Standard.Base.Nothing.Nothing|Standard.Table.Table.Table) data:Standard.Table.Table.Table
