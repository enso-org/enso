## Enso Signatures 1.0
## module Standard.Base.Data.Time.Time_Period
- type Time_Period
    - Hour
    - Microsecond
    - Millisecond
    - Minute
    - Nanosecond
    - Second
    - adjust_end self date:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - adjust_start self date:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - to_duration self -> Standard.Base.Any.Any
    - to_java_unit self -> Standard.Base.Any.Any
