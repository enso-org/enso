## Enso Signatures 1.0
## module Standard.Database.Internal.SQL_Type_Reference
- type SQL_Type_Recipe
    - Value connection:Standard.Base.Any.Any context:Standard.Database.Internal.IR.SQL_IR_Source.SQL_IR_Source expression:Standard.Database.Internal.IR.SQL_IR_Expression.SQL_IR_Expression
- type SQL_Type_Reference
    - Computed_By_Database ref:(Standard.Base.Runtime.Ref.Ref Standard.Base.Any.Any)
    - Null
    - Overridden value:Standard.Database.SQL.SQL_Type
    - cache_computed_type self ~sql_type:Standard.Database.SQL.SQL_Type -> Standard.Base.Any.Any
    - from_constant sql_type:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - get self -> Standard.Base.Any.Any
    - new connection:Standard.Base.Any.Any context:Standard.Base.Any.Any expression:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - null -> Standard.Base.Any.Any
    - to_type_override self -> Standard.Base.Any.Any
- get_or_compute ref:(Standard.Base.Runtime.Ref.Ref Standard.Base.Any.Any) -> Standard.Database.SQL.SQL_Type
