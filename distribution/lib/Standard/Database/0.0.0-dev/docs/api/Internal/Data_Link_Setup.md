## Enso Signatures 1.0
## module Standard.Database.Internal.Data_Link_Setup
- type DB_Data_Link_Type
    - Database
    - Query query:Standard.Base.Data.Text.Text
    - SQL_Statement sql_statement:Standard.Database.SQL.SQL_Statement
    - Table name:Standard.Base.Data.Text.Text
    - Table_Schema name:Standard.Base.Data.Text.Text schema:Standard.Base.Data.Text.Text
    - add_to_data_link_description self connection_description:Standard.Base.Data.Json.JS_Object -> Standard.Base.Data.Json.JS_Object
    - from_js value:Standard.Base.Any.Any -> Standard.Database.Internal.Data_Link_Setup.DB_Data_Link_Type!Standard.Base.Errors.Illegal_Argument.Illegal_Argument
    - interpret self connection:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - to_js_object self -> Standard.Base.Any.Any
- type Data_Link_Setup
    - Available create_data_link_structure:Standard.Base.Any.Any
    - Unavailable cause:Standard.Base.Data.Text.Text
    - already_a_data_link -> Standard.Database.Internal.Data_Link_Setup.Data_Link_Setup
    - save_as_data_link self destination:Standard.Base.Any.Any on_existing_file:Standard.Base.System.File.Existing_File_Behavior.Existing_File_Behavior link_type:Standard.Database.Internal.Data_Link_Setup.DB_Data_Link_Type= -> Standard.Base.Any.Any
    - save_credentials_for_data_link data_link_location:Standard.Base.Enso_Cloud.Enso_File.Enso_File credentials:Standard.Database.Connection.Credentials.Credentials -> Standard.Base.Data.Json.JS_Object
