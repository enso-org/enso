## Enso Signatures 1.0
## module Standard.Tableau.Hyper_Errors
- type Hyper_Table_Not_Found
    - Error schema:Standard.Base.Data.Text.Text name:Standard.Base.Data.Text.Text
- type Hyper_Unsupported_Type
    - Error type_name:Standard.Base.Data.Text.Text
    - to_display_text self -> Standard.Base.Any.Any
- type Query_Failed
    - Error message:Standard.Base.Data.Text.Text query:Standard.Base.Data.Text.Text
