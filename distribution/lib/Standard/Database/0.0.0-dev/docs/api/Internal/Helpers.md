## Enso Signatures 1.0
## module Standard.Database.Internal.Helpers
- assume_default_locale locale:Standard.Base.Any.Any ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
- check_connection entity1:Standard.Base.Any.Any entity2:Standard.Base.Any.Any -> Standard.Base.Data.Boolean.Boolean
- check_integrity entity1:Standard.Base.Any.Any entity2:Standard.Base.Any.Any -> Standard.Base.Data.Boolean.Boolean
- ensure_same_connection name:Standard.Base.Any.Any entities:Standard.Base.Any.Any ~continuation:Standard.Base.Any.Any -> Standard.Base.Any.Any
- expect_specific_integer_type related_column:Standard.Base.Any.Any argument:Standard.Base.Any.Any ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
- rename_internal_columns columns:Standard.Base.Any.Any new_names:Standard.Base.Any.Any -> Standard.Base.Any.Any
- unify_vector_singleton x:Standard.Base.Any.Any -> Standard.Base.Any.Any
