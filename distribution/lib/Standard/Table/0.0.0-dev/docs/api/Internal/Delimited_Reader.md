## Enso Signatures 1.0
## module Standard.Table.Internal.Delimited_Reader
- type Detected_File_Metadata
    - Value headers:(Standard.Table.Internal.Delimited_Reader.Detected_Headers|Standard.Base.Nothing.Nothing) line_separator:(Standard.Base.Data.Text.Text|Standard.Base.Nothing.Nothing) ends_with_newline:Standard.Base.Data.Boolean.Boolean has_any_content:Standard.Base.Data.Boolean.Boolean detected_encoding:Standard.Base.Data.Text.Encoding.Encoding
- type Detected_Headers
    - Existing column_names:(Standard.Base.Data.Vector.Vector Standard.Base.Data.Text.Text)
    - None column_count:Standard.Base.Data.Numbers.Integer
- default_max_columns -> Standard.Base.Any.Any
- detect_metadata file:Standard.Base.Any.Any format:Standard.Base.Any.Any -> Standard.Base.Any.Any
- handle_io_exception related_file:Standard.Base.Any.Any ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
- handle_parsing_exception -> Standard.Base.Any.Any
- handle_parsing_failure ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
- newline_at_eof file:Standard.Base.Any.Any encoding:Standard.Base.Any.Any -> Standard.Base.Any.Any
- prepare_reader format:Standard.Table.Delimited.Delimited_Format.Delimited_Format max_columns:Standard.Base.Any.Any on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior java_problem_aggregator:Standard.Base.Any.Any newline_override:Standard.Base.Any.Any= -> Standard.Base.Any.Any
- read_file format:Standard.Base.Any.Any file:Standard.Base.Any.Any on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Base.Any.Any
- read_from_reader format:Standard.Base.Any.Any java_reader:Standard.Base.Any.Any on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior max_columns:Standard.Base.Any.Any= -> Standard.Base.Any.Any
- read_stream format:Standard.Base.Any.Any stream:Standard.Base.Any.Any on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior max_columns:Standard.Base.Any.Any= related_file:Standard.Base.Any.Any= -> Standard.Base.Any.Any
- read_text text:Standard.Base.Any.Any format:Standard.Base.Any.Any on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Base.Any.Any
