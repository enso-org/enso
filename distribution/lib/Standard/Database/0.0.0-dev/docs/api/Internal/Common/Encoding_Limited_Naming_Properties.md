## Enso Signatures 1.0
## module Standard.Database.Internal.Common.Encoding_Limited_Naming_Properties
- type Encoding_Limited_Naming_Properties
    - Instance encoding:Standard.Base.Data.Text.Encoding.Encoding limit:Standard.Base.Data.Numbers.Integer is_case_sensitive:Standard.Base.Data.Boolean.Boolean=
    - encoded_size self name:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - raise_name_too_long_error self entity_kind:Standard.Base.Any.Any name:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - size_limit self -> Standard.Base.Any.Any
    - truncate self name:Standard.Base.Any.Any size:Standard.Base.Any.Any -> Standard.Base.Any.Any
