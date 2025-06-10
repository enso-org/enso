## Enso Signatures 1.0
## module Standard.Base.Data.Time.Date_Period
- type Date_Period
    - Day
    - Month
    - Quarter
    - Week first_day:Standard.Base.Data.Time.Day_Of_Week.Day_Of_Week=
    - Year
    - adjust_end self date:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - adjust_start self date:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - to_java_unit self -> Standard.Base.Any.Any
    - to_period self -> Standard.Base.Any.Any
