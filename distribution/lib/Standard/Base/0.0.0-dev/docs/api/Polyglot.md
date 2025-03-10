## Enso Signatures 1.0
## module Standard.Base.Polyglot
- type Java
    - add_to_class_path path:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - is_instance object:Standard.Base.Any.Any class:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - lookup_class name:Standard.Base.Any.Any -> Standard.Base.Any.Any
- type Polyglot
    - execute callable:Standard.Base.Any.Any arguments:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - get_array_size array:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - get_executable_name value:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - get_member object:Standard.Base.Any.Any member_name:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - get_members object:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - get_source_location value:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - has_source_location value:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - invoke target:Standard.Base.Any.Any name:Standard.Base.Any.Any arguments:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - is_language_installed language_name:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - new constructor:Standard.Base.Any.Any arguments:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - read_array_element array:Standard.Base.Any.Any index:Standard.Base.Any.Any -> Standard.Base.Any.Any
