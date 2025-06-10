## Enso Signatures 1.0
## module Standard.Database.Connection.Postgres
- type Postgres
    - Server host:Standard.Base.Data.Text.Text= port:Standard.Base.Data.Numbers.Integer= database:Standard.Base.Data.Text.Text= schema:Standard.Base.Data.Text.Text= credentials:(Standard.Database.Connection.Credentials.Credentials|Standard.Base.Nothing.Nothing)= use_ssl:Standard.Database.Connection.SSL_Mode.SSL_Mode= client_cert:(Standard.Database.Connection.Client_Certificate.Client_Certificate|Standard.Base.Nothing.Nothing)=
    - connect self options:Standard.Base.Any.Any allow_data_links:Standard.Base.Data.Boolean.Boolean= -> Standard.Base.Any.Any
    - jdbc_properties self -> Standard.Base.Any.Any
    - jdbc_url self -> Standard.Base.Any.Any
    - resolve constructor:Standard.Base.Any.Any -> Standard.Base.Any.Any
- default_postgres_database -> Standard.Base.Any.Any
- default_postgres_host -> Standard.Base.Any.Any
- default_postgres_port -> Standard.Base.Any.Any
- ssl_mode_to_jdbc_properties use_ssl:Standard.Base.Any.Any -> Standard.Base.Any.Any
