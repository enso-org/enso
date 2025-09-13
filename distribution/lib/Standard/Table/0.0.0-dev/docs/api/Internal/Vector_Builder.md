## Enso Signatures 1.0
## module Standard.Table.Internal.Vector_Builder
- type Vector_Builder
    - Append left:Standard.Base.Any.Any right:Standard.Base.Any.Any len:Standard.Base.Any.Any
    - Leaf vec:Standard.Base.Any.Any
    - ++ self other:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - build self -> Standard.Base.Any.Any
    - empty -> Standard.Base.Any.Any
    - from_vector vec:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - is_empty self -> Standard.Base.Any.Any
    - length self -> Standard.Base.Any.Any
    - not_empty self -> Standard.Base.Any.Any
