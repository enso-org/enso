## Enso Signatures 1.0
## module Standard.Database.Internal.Hidden_Table_Registry
- type Hidden_Table_Reference
    - Reference parent:Standard.Database.Internal.Hidden_Table_Registry.Hidden_Table_Registry table_name:Standard.Base.Data.Text.Text
- type Hidden_Table_Registry
    - Registry reference_counter:Standard.Database.Internal.Hidden_Table_Registry.HiddenTableReferenceCounter
    - is_registered self table_name:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - list_hidden_tables self -> Standard.Base.Any.Any
    - make_reference self table_name:Standard.Base.Any.Any -> Standard.Base.Any.Any
- dispose_reference reference:Standard.Base.Any.Any -> Standard.Base.Any.Any
- new -> Standard.Base.Any.Any
- run_maintenance_table_cleanup connection:Standard.Base.Any.Any -> Standard.Base.Any.Any
