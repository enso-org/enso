## Enso Signatures 1.0
## module Standard.Base.Data.Interval
- type Bound
    - Exclusive n:Standard.Base.Any.Any
    - Inclusive n:Standard.Base.Any.Any
- type Interval
    - Between start:Standard.Base.Data.Interval.Bound end:Standard.Base.Data.Interval.Bound
    - contains self that:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - is_empty self -> Standard.Base.Any.Any
    - new start:Standard.Base.Any.Any end:Standard.Base.Any.Any interval_type:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - not_empty self -> Standard.Base.Any.Any
    - to_display_text self -> Standard.Base.Any.Any
- type Interval_Type
    - End_Exclusive
    - Exclusive
    - Inclusive
    - Start_Exclusive
