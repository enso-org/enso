## Enso Signatures 1.0
## module Standard.Test.Test
- type Test
    - assert_no_problems value:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - build fn:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - expect_panic matcher:Standard.Base.Any.Any ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - expect_panic_with ~action:Standard.Base.Any.Any matcher:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - fail message:Standard.Base.Any.Any details:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - fail_match_on_unexpected_error error:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - with_clue ~clue:Standard.Base.Any.Any ~behavior:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - with_retries ~action:Standard.Base.Any.Any -> Standard.Base.Any.Any
