## Enso Signatures 1.0
## module Standard.AWS.Database.Redshift.Internal.Redshift_Type_Mapping
- type Redshift_Type_Mapping
    - column_fetcher_factory -> Standard.Base.Any.Any
    - error_mapper self -> Standard.Base.Any.Any
    - infer_return_type infer_from_database_callback:Standard.Base.Any.Any op_name:Standard.Base.Any.Any arguments:Standard.Base.Any.Any expression:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - is_implicit_conversion source_type:Standard.Table.Value_Type.Value_Type target_type:Standard.Table.Value_Type.Value_Type -> Standard.Base.Data.Boolean.Boolean
    - is_integer_type value_type:Standard.Table.Value_Type.Value_Type -> Standard.Base.Data.Boolean.Boolean
    - is_same_type value_type1:Standard.Table.Value_Type.Value_Type value_type2:Standard.Table.Value_Type.Value_Type -> Standard.Base.Data.Boolean.Boolean
    - prepare_type_overrides column_type_suggestions:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - should_warn_on_materialize db_type:Standard.Table.Value_Type.Value_Type in_memory_type:Standard.Table.Value_Type.Value_Type -> Standard.Base.Data.Boolean.Boolean
    - sql_type_to_text sql_type:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - sql_type_to_value_type sql_type:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - statement_setter -> Standard.Base.Any.Any
    - value_type_to_sql value_type:Standard.Base.Any.Any on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Base.Any.Any
