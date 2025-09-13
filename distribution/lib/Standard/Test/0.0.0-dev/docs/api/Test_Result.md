## Enso Signatures 1.0
## module Standard.Test.Test_Result
- type Test_Result
    - Impl group_name:Standard.Base.Data.Text.Text spec_name:Standard.Base.Data.Text.Text spec_result:Standard.Test.Spec_Result.Spec_Result time_taken:Standard.Base.Data.Time.Duration.Duration
    - is_fail self -> Standard.Base.Any.Any
    - is_pending self -> Standard.Base.Any.Any
    - is_success self -> Standard.Base.Any.Any
    - to_text self -> Standard.Base.Any.Any
