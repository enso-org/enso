## Enso Signatures 1.0
## module Standard.Base.Internal.Time.Format.Analyzer
- type Analyzer
    - Value nodes:(Standard.Base.Data.Vector.Vector Standard.Base.Any.Any) ~flattened:Standard.Base.Any.Any
    - check_24h_and_am_pm_collision self problem_builder:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - check_missing_am_pm_in_hour_parse self problem_builder:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - check_missing_year_in_date_parse self problem_builder:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - check_possible_m_mismatches self problem_builder:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - check_possible_seconds_aliasing self problem_builder:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - get_parsing_only_warnings self -> Standard.Base.Any.Any
    - has_12h self -> Standard.Base.Any.Any
    - has_24h self -> Standard.Base.Any.Any
    - has_am_pm self -> Standard.Base.Any.Any
    - has_day_and_month_but_not_year self -> Standard.Base.Any.Any
    - has_required self constructor:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - new nodes:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - validate_after_parsing self ~continuation:Standard.Base.Any.Any -> Standard.Base.Any.Any
