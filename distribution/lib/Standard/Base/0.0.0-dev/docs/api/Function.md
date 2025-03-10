## Enso Signatures 1.0
## module Standard.Base.Function
- type Function
    - << self ~that:Standard.Base.Any.Any x:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - <| self ~argument:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - >> self ~that:Standard.Base.Any.Any x:Standard.Base.Any.Any -> Standard.Base.Any.Any
- const x:Standard.Base.Any.Any ~f:Standard.Base.Any.Any -> Standard.Base.Any.Any
- curry f:Standard.Base.Any.Any x:Standard.Base.Any.Any y:Standard.Base.Any.Any -> Standard.Base.Any.Any
- flip f:Standard.Base.Any.Any x:Standard.Base.Any.Any y:Standard.Base.Any.Any -> Standard.Base.Any.Any
- identity x:Standard.Base.Any.Any -> Standard.Base.Any.Any
- uncurry f:Standard.Base.Any.Any pair:Standard.Base.Any.Any -> Standard.Base.Any.Any
