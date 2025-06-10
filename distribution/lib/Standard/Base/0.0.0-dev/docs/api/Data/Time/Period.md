## Enso Signatures 1.0
## module Standard.Base.Data.Time.Period
- type Period
    - Value internal_period:Standard.Base.Data.Time.Period.Java_Period
    - * self factor:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - + self other_period:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - - self other_period:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - between start_date_inclusive:Standard.Base.Any.Any end_date_exclusive:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - days self -> Standard.Base.Any.Any
    - months self -> Standard.Base.Any.Any
    - negate self -> Standard.Base.Any.Any
    - new years:Standard.Base.Any.Any= months:Standard.Base.Any.Any= days:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - pretty self -> Standard.Base.Any.Any
    - to_display_text self -> Standard.Base.Any.Any
    - to_js_object self -> Standard.Base.Any.Any
    - total_months self -> Standard.Base.Any.Any
    - years self -> Standard.Base.Any.Any
- catch_java_exceptions operation:Standard.Base.Any.Any ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
- ensure_period object:Standard.Base.Any.Any ~action:Standard.Base.Any.Any error_msg:Standard.Base.Any.Any= -> Standard.Base.Any.Any
