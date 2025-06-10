## Enso Signatures 1.0
## module Standard.Tableau.Hyper_File
- type Hyper_File
    - Value file:Standard.Base.System.File.File internal_schema:Standard.Base.Data.Text.Text
    - new file:Standard.Base.System.File.File schema:Standard.Base.Data.Text.Text= -> Standard.Base.Any.Any
    - read self table:Standard.Base.Data.Text.Text schema:Standard.Base.Data.Text.Text= limit:Standard.Table.Rows_To_Read.Rows_To_Read= -> Standard.Base.Any.Any
    - schema self -> Standard.Base.Any.Any
    - schemas self -> Standard.Base.Any.Any
    - set_schema self schema:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - tables self schema:Standard.Base.Data.Text.Text= -> Standard.Base.Any.Any
- make_schema_selector hyper_file:Standard.Tableau.Hyper_File.Hyper_File include_any:Standard.Base.Data.Boolean.Boolean= -> Standard.Base.Any.Any
- make_table_selector hyper_file:Standard.Tableau.Hyper_File.Hyper_File cache:Standard.Base.Any.Any= -> Standard.Base.Any.Any
- Standard.Base.Visualization.Table_Viz_Data.Table_Viz_Data.from that:Standard.Tableau.Hyper_File.Hyper_File -> Standard.Base.Visualization.Table_Viz_Data.Table_Viz_Data
