## Enso Signatures 1.0
## module Standard.Base.Data.Map
- type Map key:Standard.Base.Any.Any value:Standard.Base.Any.Any
    - empty -> Standard.Base.Any.Any
    - from_keys_and_values keys:Standard.Base.Data.Vector.Vector values:Standard.Base.Data.Vector.Vector error_on_duplicates:Standard.Base.Data.Boolean.Boolean= -> Standard.Base.Any.Any
    - from_vector vec:Standard.Base.Any.Any error_on_duplicates:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - singleton key:Standard.Base.Any.Any value:Standard.Base.Any.Any -> Standard.Base.Any.Any
