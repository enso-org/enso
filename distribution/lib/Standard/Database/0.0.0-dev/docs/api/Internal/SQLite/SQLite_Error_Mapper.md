## Enso Signatures 1.0
## module Standard.Database.Internal.SQLite.SQLite_Error_Mapper
- type SQLite_Error_Mapper
    - is_duplicate_primary_key_violation error:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - is_null_primary_key_violation error:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - is_table_already_exists_error error:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - transform_custom_errors error:Standard.Base.Any.Any -> Standard.Base.Any.Any
