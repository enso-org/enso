## Enso Signatures 1.0
## module Standard.Google.Google_Analytics
- type Google_Analytics
    - list_accounts credentials:Standard.Google.Google_Credential.Google_Credential= limit:Standard.Table.Rows_To_Read.Rows_To_Read= include_deleted:Standard.Base.Data.Boolean.Boolean= -> Standard.Base.Data.Vector.Vector
    - list_dimensions credentials:Standard.Google.Google_Credential.Google_Credential= property:Standard.Google.Google_Analytics_Property.Google_Analytics_Property= -> Standard.Base.Data.Vector.Vector
    - list_metrics credentials:Standard.Google.Google_Credential.Google_Credential= property:Standard.Google.Google_Analytics_Property.Google_Analytics_Property= -> Standard.Base.Data.Vector.Vector
    - list_properties credentials:Standard.Google.Google_Credential.Google_Credential= account:Standard.Google.Google_Analytics_Account.Google_Analytics_Account_Filter= limit:Standard.Table.Rows_To_Read.Rows_To_Read= include_deleted:Standard.Base.Data.Boolean.Boolean= -> Standard.Base.Data.Vector.Vector
    - read credentials:Standard.Google.Google_Credential.Google_Credential= property:Standard.Google.Google_Analytics_Property.Google_Analytics_Property= dimensions:Standard.Base.Data.Vector.Vector= metrics:Standard.Base.Data.Vector.Vector= start_date:Standard.Base.Data.Time.Date.Date= end_date:Standard.Base.Data.Time.Date.Date= -> Standard.Table.Table.Table
- type Google_Analytics_Error
    - to_display_text self -> Standard.Base.Any.Any
    - to_js_object self -> Standard.Base.Any.Any
    - to_text self -> Standard.Base.Any.Any
