## Enso Signatures 1.0
## module Standard.Base.Polyglot
- type Java
    - add_to_class_path path:Standard.Base.Data.Text.Text -> Standard.Base.Nothing.Nothing
    - is_instance object:Standard.Base.Any.Any class:Standard.Base.Any.Any -> Standard.Base.Data.Boolean.Boolean
    - lookup_class name:Standard.Base.Data.Text.Text -> Standard.Base.Any.Any
- type Polyglot
    - execute callable:Standard.Base.Any.Any arguments:(Standard.Base.Data.Vector.Vector|Standard.Base.Data.Array.Array) -> Standard.Base.Any.Any
    - get_array_size array:Standard.Base.Any.Any -> Standard.Base.Data.Numbers.Integer
    - get_executable_name value:Standard.Base.Any.Any -> Standard.Base.Data.Text.Text
    - get_member object:Standard.Base.Any.Any member_name:Standard.Base.Data.Text.Text -> Standard.Base.Any.Any
    - get_members object:Standard.Base.Any.Any -> (Standard.Base.Data.Vector.Vector Standard.Base.Data.Text.Text)
    - get_source_location value:Standard.Base.Any.Any -> Standard.Base.Runtime.Source_Location.Source_Location
    - has_source_location value:Standard.Base.Any.Any -> Standard.Base.Data.Boolean.Boolean
    - invoke target:Standard.Base.Any.Any name:Standard.Base.Data.Text.Text arguments:(Standard.Base.Data.Vector.Vector|Standard.Base.Data.Array.Array) -> Standard.Base.Any.Any
    - is_language_installed is_language_installed:Standard.Base.Data.Text.Text -> Standard.Base.Data.Boolean.Boolean
    - new constructor:Standard.Base.Any.Any arguments:(Standard.Base.Data.Vector.Vector|Standard.Base.Data.Array.Array) -> Standard.Base.Any.Any
    - read_array_element array:Standard.Base.Any.Any index:Standard.Base.Data.Numbers.Integer -> Standard.Base.Any.Any
