## Enso Signatures 1.0
## module Standard.Database.Internal.Table_Naming_Helper
- type Table_Naming_Helper
    - Value connection:Standard.Base.Any.Any
    - create_unique_name_strategy self -> Standard.Base.Any.Any
    - generate_dry_run_table_name self table_name:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - generate_random_table_name self prefix:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - is_table_name_valid self table_name:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - naming_properties self -> Standard.Base.Any.Any
    - truncate_if_needed self name:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - verify_table_name self table_name:Standard.Base.Any.Any ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
