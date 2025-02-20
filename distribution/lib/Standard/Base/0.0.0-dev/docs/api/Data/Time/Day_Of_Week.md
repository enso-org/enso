## Enso Signatures 1.0
## module Standard.Base.Data.Time.Day_Of_Week
- type Day_Of_Week
    - Friday
    - Monday
    - Saturday
    - Sunday
    - Thursday
    - Tuesday
    - Wednesday
    - to_integer self first_day:Standard.Base.Any.Any= start_at_zero:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - to_java self -> Standard.Base.Any.Any
- type Day_Of_Week_Comparator
    - compare x:Standard.Base.Any.Any y:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - hash x:Standard.Base.Any.Any -> Standard.Base.Any.Any
- Standard.Base.Data.Ordering.Comparable.from that:Standard.Base.Data.Time.Day_Of_Week.Day_Of_Week -> Standard.Base.Data.Ordering.Comparable
- Standard.Base.Data.Time.Day_Of_Week.Day_Of_Week.from that:Standard.Base.Data.Numbers.Integer first_day:Standard.Base.Data.Time.Day_Of_Week.Day_Of_Week= start_at_zero:Standard.Base.Data.Boolean.Boolean= -> Standard.Base.Data.Time.Day_Of_Week.Day_Of_Week
