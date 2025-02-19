## Enso Signatures 1.0
## module Standard.Base.Runtime.Ref
- type Ref
    - get self -> Standard.Base.Any.Any
    - modify self fun:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - new value:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - put self new_value:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - with_modification self modifier:Standard.Base.Any.Any ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - with_value self new_value:Standard.Base.Any.Any ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
