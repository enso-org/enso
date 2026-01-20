## Enso Signatures 1.0
## module Standard.DuckDB.DuckDB
- type DuckDB
    - From_File location:Standard.Base.System.File.File= schema:Standard.Base.Data.Text.Text= read_only:Standard.Base.Data.Boolean.Boolean=
    - In_Memory schema:Standard.Base.Data.Text.Text= read_only:Standard.Base.Data.Boolean.Boolean=
    - connect self options:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - jdbc_properties self -> Standard.Base.Any.Any
    - jdbc_url self -> Standard.Base.Any.Any
    - resolve constructor:Standard.Base.Any.Any -> Standard.Base.Any.Any
