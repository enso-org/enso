## Enso Signatures 1.0
## module Standard.Google.Google_Analytics_Account
- type Google_Analytics_Account
    - create_time self -> Standard.Base.Any.Any
    - id self -> Standard.Base.Any.Any
    - is_deleted self -> Standard.Base.Any.Any
    - name self -> Standard.Base.Any.Any
    - properties self credentials:Standard.Google.Google_Credential.Google_Credential= limit:Standard.Table.Rows_To_Read.Rows_To_Read= include_deleted:Standard.Base.Data.Boolean.Boolean= -> Standard.Base.Any.Any
    - region_code self -> Standard.Base.Any.Any
    - to_display_text self -> Standard.Base.Any.Any
    - to_js_object self -> Standard.Base.Any.Any
    - to_text self -> Standard.Base.Any.Any
- type Google_Analytics_Account_Filter
    - Account account:(Standard.Base.Data.Text.Text|Standard.Google.Google_Analytics_Account.Google_Analytics_Account)
    - Accounts accounts:Standard.Base.Data.Vector.Vector
    - All_Accounts
    - default_widget self_arg:Standard.Base.Any.Any cache:Standard.Base.Any.Any display:Standard.Base.Metadata.Display= -> Standard.Base.Metadata.Widget
- Standard.Google.Google_Analytics_Account.Google_Analytics_Account_Filter.from that:Standard.Google.Google_Analytics_Account.Google_Analytics_Account -> Standard.Google.Google_Analytics_Account.Google_Analytics_Account_Filter
- Standard.Google.Google_Analytics_Account.Google_Analytics_Account_Filter.from that:Standard.Base.Data.Text.Text -> Standard.Google.Google_Analytics_Account.Google_Analytics_Account_Filter
- Standard.Google.Google_Analytics_Account.Google_Analytics_Account_Filter.from that:Standard.Base.Data.Vector.Vector -> Standard.Google.Google_Analytics_Account.Google_Analytics_Account_Filter
