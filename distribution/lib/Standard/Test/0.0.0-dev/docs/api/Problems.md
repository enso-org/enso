## Enso Signatures 1.0
## module Standard.Test.Problems
- assume_no_problems result:Standard.Base.Any.Any -> Standard.Base.Any.Any
- expect_only_warning expected_warning:Standard.Base.Any.Any result:Standard.Base.Any.Any unwrap_errors:Standard.Base.Any.Any= -> Standard.Base.Any.Any
- expect_warning expected_warning:Standard.Base.Any.Any result:Standard.Base.Any.Any unwrap_errors:Standard.Base.Any.Any= -> Standard.Base.Any.Any
- get_attached_warnings v:Standard.Base.Any.Any -> Standard.Base.Any.Any
- not_expect_warning expected_warning_type:Standard.Base.Any.Any result:Standard.Base.Any.Any -> Standard.Base.Any.Any
- test_advanced_problem_handling action:Standard.Base.Any.Any error_checker:Standard.Base.Any.Any warnings_checker:Standard.Base.Any.Any result_checker:Standard.Base.Any.Any -> Standard.Base.Any.Any
- test_problem_handling action:Standard.Base.Any.Any expected_problems:Standard.Base.Any.Any result_checker:Standard.Base.Any.Any unwrap_errors:Standard.Base.Data.Boolean.Boolean= ignore_warning_cardinality:Standard.Base.Data.Boolean.Boolean= -> Standard.Base.Any.Any
