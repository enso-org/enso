## Enso Signatures 1.0
## module Standard.Microsoft.Connection.SQLServer_Details
- type SQLServer_Details
    - SQLServer host:Standard.Base.Data.Text.Text= credentials:Standard.Microsoft.Connection.SQLServer_Credentials.SQLServer_Credentials= port:Standard.Base.Data.Numbers.Integer= database:Standard.Base.Data.Text.Text=
    - connect self options:Standard.Base.Any.Any allow_data_links:Standard.Base.Data.Boolean.Boolean= -> Standard.Base.Any.Any
    - jdbc_properties self -> Standard.Base.Any.Any
    - jdbc_url self -> Standard.Base.Any.Any
    - resolve constructor:Standard.Base.Any.Any -> Standard.Base.Any.Any
