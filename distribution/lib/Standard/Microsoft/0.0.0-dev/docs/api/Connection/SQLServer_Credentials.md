## Enso Signatures 1.0
## module Standard.Microsoft.Connection.SQLServer_Credentials
- type SQLServer_Credentials
    - Active_Directory_Default
    - Active_Directory_Integrated
    - Active_Directory_Password username:(Standard.Base.Data.Text.Text|Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret)= password:(Standard.Base.Data.Text.Text|Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret)=
    - Active_Directory_Service_Principal principal_id:(Standard.Base.Data.Text.Text|Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret)= principal_secret:(Standard.Base.Data.Text.Text|Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret)=
    - None
    - Username_And_Password username:(Standard.Base.Data.Text.Text|Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret)= password:(Standard.Base.Data.Text.Text|Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret)=
    - to_jdbc_properties self -> Standard.Base.Any.Any
    - to_text self -> Standard.Base.Any.Any
- Standard.Microsoft.Connection.SQLServer_Credentials.SQLServer_Credentials.from that:Standard.Base.Nothing.Nothing -> Standard.Microsoft.Connection.SQLServer_Credentials.SQLServer_Credentials
- Standard.Microsoft.Connection.SQLServer_Credentials.SQLServer_Credentials.from that:Standard.Database.Connection.Credentials.Credentials -> Standard.Microsoft.Connection.SQLServer_Credentials.SQLServer_Credentials
