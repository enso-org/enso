## Enso Signatures 1.0
## module Standard.Table.Internal.Delimited_Writer
- append_to_file table:Standard.Base.Any.Any format:Standard.Base.Any.Any file:Standard.Base.System.File.Generic.Writable_File.Writable_File match_columns:Standard.Base.Any.Any on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Base.Any.Any
- append_to_local_file table:Standard.Base.Any.Any format:Standard.Base.Any.Any file:Standard.Base.System.File.File match_columns:Standard.Base.Any.Any on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Base.Any.Any
- default_line_separator_for_writing -> Standard.Base.Any.Any
- should_write_headers headers:Standard.Table.Headers.Headers -> Standard.Base.Any.Any
- write_file table:Standard.Base.Any.Any format:Standard.Base.Any.Any file:Standard.Base.System.File.Generic.Writable_File.Writable_File on_existing_file:Standard.Base.Any.Any match_columns:Standard.Base.Any.Any on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Base.Any.Any
- write_text table:Standard.Base.Any.Any format:Standard.Base.Any.Any -> Standard.Base.Any.Any
- write_to_stream table:Standard.Base.Any.Any format:Standard.Base.Any.Any stream:Standard.Base.Any.Any on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior related_file:Standard.Base.Any.Any= separator_override:Standard.Base.Any.Any= needs_leading_newline:Standard.Base.Any.Any= -> Standard.Base.Any.Any
- write_to_writer table:Standard.Base.Any.Any format:Standard.Base.Any.Any java_writer:Standard.Base.Any.Any separator_override:Standard.Base.Any.Any= needs_leading_newline:Standard.Base.Any.Any= -> Standard.Base.Any.Any
