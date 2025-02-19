## Enso Signatures 1.0
## module Standard.Base.Widget_Helpers
- make_all_selector display:Standard.Base.Metadata.Display= -> Standard.Base.Metadata.Widget
- make_any_selector display:Standard.Base.Metadata.Display= add_text:Standard.Base.Data.Boolean.Boolean= add_regex:Standard.Base.Data.Boolean.Boolean= add_named_pattern:Standard.Base.Data.Boolean.Boolean= add_number:Standard.Base.Data.Boolean.Boolean= add_boolean:Standard.Base.Data.Boolean.Boolean= add_date:Standard.Base.Data.Boolean.Boolean= add_time:Standard.Base.Data.Boolean.Boolean= add_date_time:Standard.Base.Data.Boolean.Boolean= add_nothing:Standard.Base.Data.Boolean.Boolean= -> Standard.Base.Any.Any
- make_data_cleanse_vector_selector display:Standard.Base.Metadata.Display= -> Standard.Base.Any.Any
- make_date_format_selector date:Standard.Base.Data.Time.Date.Date= -> Standard.Base.Any.Any
- make_date_time_format_selector date_time:Standard.Base.Data.Time.Date_Time.Date_Time= -> Standard.Base.Any.Any
- make_delimiter_selector -> Standard.Base.Any.Any
- make_file_read_delimiter_selector -> Standard.Base.Any.Any
- make_format_chooser include_number:Standard.Base.Data.Boolean.Boolean= include_date:Standard.Base.Data.Boolean.Boolean= include_date_time:Standard.Base.Data.Boolean.Boolean= include_time:Standard.Base.Data.Boolean.Boolean= include_boolean:Standard.Base.Data.Boolean.Boolean= -> Standard.Base.Any.Any
- make_number_format_selector value:Standard.Base.Data.Numbers.Number= -> Standard.Base.Any.Any
- make_regex_text_widget display:Standard.Base.Metadata.Display= -> Standard.Base.Any.Any
- make_text_secret_selector display:Standard.Base.Metadata.Display= -> Standard.Base.Metadata.Widget
- make_time_format_selector time:Standard.Base.Data.Time.Time_Of_Day.Time_Of_Day= -> Standard.Base.Any.Any
