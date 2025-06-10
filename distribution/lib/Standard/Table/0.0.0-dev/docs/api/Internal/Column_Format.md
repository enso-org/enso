## Enso Signatures 1.0
## module Standard.Table.Internal.Column_Format
- handle_illegal_argument_exception format_string:Standard.Base.Any.Any ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
- make_boolean_formatter format:(Standard.Base.Data.Text.Text|Standard.Base.Nothing.Nothing) -> Standard.Base.Any.Any
- make_datetime_formatter locale_override:Standard.Base.Data.Locale.Locale format:(Standard.Base.Data.Text.Text|Standard.Base.Data.Time.Date_Time_Formatter.Date_Time_Formatter|Standard.Base.Nothing.Nothing) -> Standard.Base.Any.Any
- make_value_formatter locale:Standard.Base.Any.Any format:(Standard.Base.Data.Text.Text|Standard.Base.Nothing.Nothing) -> Standard.Base.Any.Any
- make_value_formatter_for_value_type value_type:Standard.Base.Any.Any locale:Standard.Base.Any.Any -> Standard.Base.Any.Any
