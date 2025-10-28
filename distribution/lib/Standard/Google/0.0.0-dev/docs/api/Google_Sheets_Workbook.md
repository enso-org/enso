## Enso Signatures 1.0
## module Standard.Google.Google_Sheets_Workbook
- type Google_API_Error
    - Error details:Standard.Base.Any.Any
    - to_display_text self -> Standard.Base.Any.Any
- type Google_Sheets_Workbook
    - Value workbook_id:Standard.Base.Data.Text.Text java_service:Standard.Base.Any.Any
    - named_ranges self -> (Standard.Base.Data.Vector.Vector Standard.Base.Data.Text.Text)
    - named_ranges_count self -> Standard.Base.Data.Numbers.Integer
    - new workbook_id:Standard.Base.Data.Text.Text credentials:Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret -> Standard.Base.Any.Any
    - read self query:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer) headers:Standard.Table.Headers.Headers= limit:Standard.Table.Rows_To_Read.Rows_To_Read= skip_rows:Standard.Base.Data.Numbers.Integer= on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior= -> Standard.Table.Table.Table
    - read_many self sheet_names:Standard.Base.Data.Vector.Vector= headers:Standard.Table.Headers.Headers= return:Standard.Table.Return_As_Table.Return_As_Table= on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior= -> Standard.Base.Any.Any
    - sheet_count self -> Standard.Base.Data.Numbers.Integer
    - sheet_names self -> (Standard.Base.Data.Vector.Vector Standard.Base.Data.Text.Text)
- Standard.Base.Visualization.Table_Viz_Data.Table_Viz_Data.from that:Standard.Google.Google_Sheets_Workbook.Google_Sheets_Workbook -> Standard.Base.Visualization.Table_Viz_Data.Table_Viz_Data
