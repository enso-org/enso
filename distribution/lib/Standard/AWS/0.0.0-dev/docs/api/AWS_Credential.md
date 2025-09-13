## Enso Signatures 1.0
## module Standard.AWS.AWS_Credential
- type AWS_Credential
    - Default
    - Key access_key_id:(Standard.Base.Data.Text.Text|Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret)= secret_access_key:(Standard.Base.Data.Text.Text|Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret)=
    - Profile profile:Standard.Base.Data.Text.Text=
    - With_Configuration base_credential:Standard.AWS.AWS_Credential.AWS_Credential default_region:Standard.AWS.AWS_Region.AWS_Region
    - as_java self -> Standard.Base.Any.Any
    - default_widget add_user_password:Standard.Base.Data.Boolean.Boolean= display:Standard.Base.Metadata.Display= -> Standard.Base.Metadata.Widget
    - get_default_region self -> Standard.AWS.AWS_Region.AWS_Region
    - is_default_credential_available -> Standard.Base.Data.Boolean.Boolean
    - profile_names -> Standard.Base.Any.Any
    - to_display_text self -> Standard.Base.Data.Text.Text
    - to_text self -> Standard.Base.Data.Text.Text
    - with_default_override override:Standard.AWS.AWS_Credential.AWS_Credential ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - with_default_region self region:Standard.AWS.AWS_Region.AWS_Region= -> Standard.AWS.AWS_Credential.AWS_Credential
