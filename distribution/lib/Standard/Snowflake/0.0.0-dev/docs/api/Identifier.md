## Enso Signatures 1.0
## module Standard.Snowflake.Identifier
- type Identifier
    - Name name:Standard.Base.Data.Text.Text=
    - Name_And_Schema name:Standard.Base.Data.Text.Text= schema:Standard.Base.Data.Text.Text=
    - to_identifier self -> Standard.Base.Any.Any
    - to_option self -> Standard.Base.Any.Any
- Standard.Snowflake.Identifier.Identifier.from that:Standard.Base.Data.Text.Text -> Standard.Snowflake.Identifier.Identifier
