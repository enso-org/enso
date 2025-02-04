## Enso Signatures 1.0
## module Standard.Base.Data.Text.Regex.Internal.Replacer
- type Replacement
    - Literal text:Standard.Base.Data.Text.Text
    - Substitution group_number:Standard.Base.Data.Numbers.Integer
- type Replacer
    - Value replacement:(Standard.Base.Data.Vector.Vector Standard.Base.Data.Text.Regex.Internal.Replacer.Replacement)
    - new replacement_string:Standard.Base.Any.Any pattern:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - replace self match:Standard.Base.Any.Any -> Standard.Base.Any.Any
- build_replacement_vector replacement_string:Standard.Base.Any.Any pattern:Standard.Base.Any.Any -> Standard.Base.Any.Any
- build_replacement_vector_cached replacement_string:Standard.Base.Any.Any pattern:Standard.Base.Any.Any -> Standard.Base.Any.Any
- get_lru_size -> Standard.Base.Any.Any
- group_reference_regex -> Standard.Base.Any.Any
- parse_group_number pattern:Standard.Base.Any.Any match:Standard.Base.Any.Any -> Standard.Base.Any.Any
- replacer_cache_lookup replacement_string:Standard.Base.Any.Any -> Standard.Base.Any.Any
