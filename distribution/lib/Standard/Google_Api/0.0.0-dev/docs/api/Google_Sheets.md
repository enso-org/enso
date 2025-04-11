## Enso Signatures 1.0
## module Standard.Google_Api.Google_Sheets
- type Google_Api_Error
    - Error message:Standard.Base.Any.Any cause:Standard.Base.Any.Any
- type Google_Sheets
    - get_sheet_names self workbook_id:Standard.Base.Data.Text.Text -> Standard.Base.Any.Any
    - get_table self workbook_id:Standard.Base.Data.Text.Text sheet_range:Standard.Base.Data.Text.Text -> Standard.Table.Table.Table
    - initialize credentials:Standard.Base.Any.Any -> Standard.Base.Any.Any
