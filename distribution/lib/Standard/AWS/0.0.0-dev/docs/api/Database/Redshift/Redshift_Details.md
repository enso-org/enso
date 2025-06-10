## Enso Signatures 1.0
## module Standard.AWS.Database.Redshift.Redshift_Details
- type Redshift_Details
    - Redshift host:Standard.Base.Data.Text.Text= port:Standard.Base.Data.Numbers.Integer= schema:Standard.Base.Data.Text.Text= db_user:Standard.Base.Data.Text.Text= credentials:(Standard.Database.Connection.Credentials.Credentials|Standard.AWS.AWS_Credential.AWS_Credential)= use_ssl:Standard.Database.Connection.SSL_Mode.SSL_Mode= client_cert:(Standard.Database.Connection.Client_Certificate.Client_Certificate|Standard.Base.Nothing.Nothing)=
    - connect self options:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - jdbc_properties self -> Standard.Base.Any.Any
    - jdbc_url self -> Standard.Base.Any.Any
    - resolve constructor:Standard.Base.Any.Any -> Standard.Base.Any.Any
