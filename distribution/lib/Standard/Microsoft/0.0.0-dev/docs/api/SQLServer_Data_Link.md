## Enso Signatures 1.0
## module Standard.Microsoft.SQLServer_Data_Link
- type SQLServer_Data_Link
    - Value details:Standard.Microsoft.Connection.SQLServer_Details.SQLServer_Details source:Standard.Base.Enso_Cloud.Data_Link_Helpers.Data_Link_Source_Metadata link_type:Standard.Database.Internal.Data_Link_Setup.DB_Data_Link_Type
    - parse json:Standard.Base.Any.Any source:Standard.Base.Any.Any -> Standard.Microsoft.SQLServer_Data_Link.SQLServer_Data_Link
    - read self format:Standard.Base.Any.Any= on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Base.Any.Any
