## Enso Signatures 1.0
## module Standard.Database.Connection.Credentials
- type Credentials
    - Username_And_Password username:(Standard.Base.Data.Text.Text|Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret)= password:(Standard.Base.Data.Text.Text|Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret)=
    - default_widget include_nothing:Standard.Base.Data.Boolean.Boolean= display:Standard.Base.Metadata.Display= -> Standard.Base.Metadata.Widget
    - to_display_text self -> Standard.Base.Data.Text.Text
    - to_text self -> Standard.Base.Any.Any
