## Enso Signatures 1.0
## module Standard.Google_Api.Google_Sheets_Workbook
- type Google_Api_Error
    - Error message:Standard.Base.Any.Any cause:Standard.Base.Any.Any
- type Google_Sheets_Workbook
    - Value workbook_id:Standard.Base.Data.Text.Text java_service:Standard.Base.Any.Any
    - new workbook_id:Standard.Base.Data.Text.Text credentials:Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret -> Standard.Base.Any.Any
    - read self query:Standard.Base.Data.Text.Text -> Standard.Table.Table.Table
    - sheet_names self -> Standard.Base.Any.Any
- Standard.Base.Visualization.Table_Viz_Data.Table_Viz_Data.from that:Standard.Google_Api.Google_Sheets_Workbook.Google_Sheets_Workbook -> Standard.Base.Visualization.Table_Viz_Data.Table_Viz_Data
