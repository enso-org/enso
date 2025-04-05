## Enso Signatures 1.0
## module Standard.Snowflake.Connection.Snowflake_Details
- type Snowflake_Details
    - Snowflake account:Standard.Base.Data.Text.Text= credentials:(Standard.Database.Connection.Credentials.Credentials|Standard.Snowflake.Connection.Key_Pair_Credentials.Key_Pair_Credentials|Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret)= database:Standard.Base.Data.Text.Text= schema:Standard.Base.Data.Text.Text= warehouse:Standard.Base.Data.Text.Text=
    - connect self options:Standard.Base.Any.Any allow_data_links:Standard.Base.Data.Boolean.Boolean= -> Standard.Base.Any.Any
    - jdbc_url self -> Standard.Base.Any.Any
    - resolve constructor:Standard.Base.Any.Any -> Standard.Base.Any.Any
