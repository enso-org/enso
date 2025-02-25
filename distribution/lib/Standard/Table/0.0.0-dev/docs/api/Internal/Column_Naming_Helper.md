## Enso Signatures 1.0
## module Standard.Table.Internal.Column_Naming_Helper
- type Column_Naming_Helper
    - Value naming_properties:Standard.Base.Any.Any generated_name_properties:Standard.Base.Any.Any=
    - binary_operation_name self operation_name:Standard.Base.Any.Any left:Standard.Base.Any.Any right:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - check_ambiguity self existing_names:Standard.Base.Any.Any new_name:Standard.Base.Any.Any ~continuation:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - concat self texts:Standard.Base.Any.Any add_spaces:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - create_unique_name_strategy self -> Standard.Base.Any.Any
    - ensure_name_is_valid self name:Standard.Base.Any.Any ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - function_name self function_name:Standard.Base.Any.Any arguments:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - has_length_limit self -> Standard.Base.Any.Any
    - in_memory -> Standard.Base.Any.Any
    - make_temp_column_name self existing_column_names:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - sanitize_name self name:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - to_expression_text self value:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - validate_many_column_names self names:Standard.Base.Any.Any ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
