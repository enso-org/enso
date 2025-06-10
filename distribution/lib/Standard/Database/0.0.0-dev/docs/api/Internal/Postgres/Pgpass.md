## Enso Signatures 1.0
## module Standard.Database.Internal.Postgres.Pgpass
- type Pgpass_Entry
    - Value host:Standard.Base.Any.Any port:Standard.Base.Any.Any database:Standard.Base.Any.Any username:Standard.Base.Any.Any password:Standard.Base.Any.Any
    - matches self host:Standard.Base.Any.Any port:Standard.Base.Any.Any database:Standard.Base.Any.Any username:Standard.Base.Any.Any= -> Standard.Base.Any.Any
- locate -> Standard.Base.Any.Any
- parse_file file:Standard.Base.Any.Any -> Standard.Base.Any.Any
- parse_line line:Standard.Base.Any.Any -> Standard.Base.Any.Any
- read host:Standard.Base.Any.Any port:Standard.Base.Any.Any database:Standard.Base.Any.Any username:Standard.Base.Any.Any= -> Standard.Base.Any.Any
- verify file:Standard.Base.Any.Any -> Standard.Base.Any.Any
