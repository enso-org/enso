## Enso Signatures 1.0
## module Standard.Table.Extensions.Table_Conversions
- append_to_json_table file:Standard.Base.System.File.File table:Standard.Base.Any.Any on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior -> Standard.Base.Any.Any
- Standard.Base.Data.Time.Date_Range.Date_Range.to_table self name:Standard.Base.Data.Text.Text= -> Standard.Base.Any.Any
- Standard.Base.System.File_Format.JSON_Format.write_table self file:Standard.Base.System.File.Generic.Writable_File.Writable_File table:Standard.Base.Any.Any on_existing_file:Standard.Base.Any.Any match_columns:Standard.Base.Any.Any on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior= -> Standard.Base.Any.Any
- Standard.Base.System.File_Format.JSON_Lines_Format.write_table self file:Standard.Base.System.File.Generic.Writable_File.Writable_File table:Standard.Base.Any.Any on_existing_file:Standard.Base.Any.Any match_columns:Standard.Base.Any.Any on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior= -> Standard.Base.Any.Any
- Standard.Base.Data.Json.JS_Object.to_table self fields:Standard.Base.Any.Any= -> Standard.Base.Any.Any
- Standard.Base.Data.Range.Range.to_table self name:Standard.Base.Data.Text.Text= -> Standard.Base.Any.Any
- Standard.Table.Table.Table.from_objects value:Standard.Base.Any.Any fields:(Standard.Base.Data.Vector.Vector|Standard.Base.Nothing.Nothing)= -> Standard.Base.Any.Any
- Standard.Base.Data.Text.Text.parse_to_table self pattern:(Standard.Base.Data.Text.Text|Standard.Base.Data.Text.Regex.Regex) case_sensitivity:Standard.Base.Data.Text.Case_Sensitivity.Case_Sensitivity= parse_values:Standard.Base.Any.Any= on_problems:Standard.Base.Errors.Problem_Behavior.Problem_Behavior= -> Standard.Base.Any.Any
- Standard.Base.Data.Vector.Vector.to_table self fields:Standard.Base.Any.Any= -> Standard.Base.Any.Any
- Standard.Base.Data.XML.XML_Document.to_table self -> Standard.Base.Any.Any
- Standard.Base.Data.XML.XML_Element.to_table self -> Standard.Base.Any.Any
