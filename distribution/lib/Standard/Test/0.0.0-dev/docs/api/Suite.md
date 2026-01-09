## Enso Signatures 1.0
## module Standard.Test.Suite
- type Suite
    - group_names self -> Standard.Base.Any.Any
    - print_all self -> Standard.Base.Any.Any
    - run_with_filter self filter:(Standard.Base.Data.Text.Text|Standard.Base.Nothing.Nothing)= should_exit:Standard.Base.Data.Boolean.Boolean= -> (Standard.Base.Data.Boolean.Boolean|Standard.Base.Nothing.Nothing)
- type Suite_Builder
    - group self name:Standard.Base.Data.Text.Text fn:Standard.Base.Any.Any ~pending:(Standard.Base.Data.Text.Text|Standard.Base.Nothing.Nothing)= -> Standard.Base.Any.Any
