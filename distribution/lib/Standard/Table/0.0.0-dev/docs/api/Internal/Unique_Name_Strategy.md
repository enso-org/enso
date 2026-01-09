## Enso Signatures 1.0
## module Standard.Table.Internal.Unique_Name_Strategy
- type Unique_Name_Strategy
    - Value deduplicator:Standard.Base.Any.Any
    - combine_with_prefix self first:Standard.Base.Any.Any second:Standard.Base.Any.Any second_prefix:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - invalid_names self -> Standard.Base.Any.Any
    - is_unique self name:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - make_all_unique self names:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - make_unique self name:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - make_valid_name self input:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - mark_used self names:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - new naming_properties:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - renames self -> Standard.Base.Any.Any
    - truncated_names self -> (Standard.Base.Data.Dictionary.Dictionary Standard.Base.Data.Text.Text Standard.Base.Data.Text.Text)
