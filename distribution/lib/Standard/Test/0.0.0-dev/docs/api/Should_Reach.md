## Enso Signatures 1.0
## module Standard.Test.Should_Reach
- type Should_Reach
    - new -> Standard.Base.Any.Any
    - reached self -> Standard.Base.Nothing.Nothing
    - should_have_reached self frames_to_skip:Standard.Base.Data.Numbers.Integer= -> Standard.Test.Spec_Result.Spec_Result
