## Enso Signatures 1.0
## module Standard.Snowflake.Connection.Key_Pair_Credentials
- type Generated_Key_Pair
    - alter_user_query self username:(Standard.Base.Data.Text.Text|Standard.Base.Nothing.Nothing)= -> Standard.Base.Data.Text.Text
    - public_key_file self -> Standard.Base.Enso_Cloud.Enso_File.Enso_File
    - to_display_text self -> Standard.Base.Data.Text.Text
    - to_js_object self -> Standard.Base.Any.Any
    - to_text self -> Standard.Base.Data.Text.Text
- type Key_Pair_Credentials
    - Key_Pair username:Standard.Base.Data.Text.Text= private_key:(Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret|Standard.Base.System.File.File|Standard.Base.Enso_Cloud.Enso_File.Enso_File)= passphrase:(Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret|Standard.Base.Data.Text.Text)=
    - generate_key_pair location:Standard.Base.Enso_Cloud.Enso_File.Enso_File= name:Standard.Base.Data.Text.Text if_exists:Standard.Snowflake.Connection.Key_Pair_Credentials.On_Existing_Key_Pair= -> Standard.Snowflake.Connection.Key_Pair_Credentials.Generated_Key_Pair
- type On_Existing_Key_Pair
    - Error
    - Overwrite
    - Use_Existing
- generate_alter_user_query username:(Standard.Base.Data.Text.Text|Standard.Base.Nothing.Nothing) public_key_file_content:Standard.Base.Data.Text.Text -> Standard.Base.Data.Text.Text
- Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret.from that:Standard.Snowflake.Connection.Key_Pair_Credentials.Generated_Key_Pair -> Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret
