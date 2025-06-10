## Enso Signatures 1.0
## module Standard.Database.SQL
- type SQL_Builder
    - ++ self other:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - build self -> Standard.Base.Any.Any
    - code text:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - empty -> Standard.Base.Any.Any
    - extract_constant self -> Standard.Base.Any.Any
    - from_fragments fragments:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - interpolation object:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - is_constant self -> Standard.Base.Any.Any
    - is_empty self -> Standard.Base.Any.Any
    - join separator:Standard.Base.Any.Any statements:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - paren self -> Standard.Base.Any.Any
    - prefix_if_present self prefix:Standard.Base.Any.Any -> Standard.Base.Any.Any
- type SQL_Fragment
- optimize_fragments fragments:Standard.Base.Any.Any -> Standard.Base.Any.Any
