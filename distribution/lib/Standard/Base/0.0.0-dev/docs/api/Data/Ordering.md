## Enso Signatures 1.0
## module Standard.Base.Data.Ordering
- type Comparable
    - new value:Standard.Base.Any.Any comparator:Standard.Base.Any.Any -> Standard.Base.Data.Ordering.Comparable
- Standard.Base.Data.Ordering.Ordering.and_then self ~other:Standard.Base.Any.Any -> Standard.Base.Any.Any
- Standard.Base.Data.Ordering.Comparable.from that:Standard.Base.Any.Any -> Standard.Base.Data.Ordering.Comparable
- type Ordering
    - Equal
    - Greater
    - Less
    - and_then self ~other:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - compare x:Standard.Base.Any.Any y:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - from_sign sign:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - hash x:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - to_sign self -> Standard.Base.Any.Any
- Standard.Base.Data.Ordering.Comparable.from that:Standard.Base.Data.Ordering.Ordering -> Standard.Base.Data.Ordering.Comparable
