## Enso Signatures 1.0
## module Standard.Database.Connection.SQLite
- type SQLite
    - From_File location:Standard.Base.System.File.File=
    - In_Memory
    - connect self options:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - jdbc_properties self -> Standard.Base.Any.Any
    - jdbc_url self -> Standard.Base.Any.Any
    - resolve constructor:Standard.Base.Any.Any -> Standard.Base.Any.Any
