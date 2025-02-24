"""
Tests for parsing and gathering the new JSON data format introduced in https://github.com/enso-org/enso/pull/10224
The schema is specified as: https://raw.githubusercontent.com/enso-org/enso/6732a5e7e94ad3395c49627fa2d3417d4a7fcd68/lib/java/benchmarks-common/src/main/resources/results_schema.json
"""

import unittest
from bench_tool import Author, Commit, JobRun, JsonJobReport
import tempfile

from bench_tool.bench_results import _parse_bench_report_from_json

SAMPLE_REPORT = """
{
  "$schema" : "https://raw.githubusercontent.com/enso-org/engine-benchmark-results/fef8b37881580512435cfa9bb2b867e6b97e147a/schema/cache-v2.json",
  "configuration" : {
    "osName" : "Linux",
    "osArch" : "amd64",
    "osVersion" : "6.8.0-53-generic",
    "vmName" : "OpenJDK 64-Bit Server VM",
    "vmVersion" : "21.0.2+13-jvmci-23.1-b30",
    "vmVendor" : "GraalVM Community",
    "jdkVersion" : "21.0.2"
  },
  "ghActionRun" : null,
  "results" : [ {
    "label" : "org.enso.interpreter.bench.benchmarks.semantic.WarningBenchmarks.sameWarningVecSum",
    "timestamp" : "2025-02-24T18:04:13Z",
    "score" : 212.37731426666667,
    "samples" : 1,
    "warmupIterations" : 1,
    "warmupMillis" : 1000,
    "measureIterations" : 1,
    "measureMillis" : 3000,
    "commitId" : "a2e241baac3cd4566e0e243ae72ce015848744c4",
    "branch" : "wip/akirathan/9355-bench-res-json",
    "measurementStatistics" : {
      "stddev" : "NaN",
      "mean" : 212.37731426666667,
      "min" : 212.37731426666667,
      "max" : 212.37731426666667,
      "error50" : "NaN",
      "error95" : "NaN",
      "percentiles" : [ {
        "value" : 212.37731426666667,
        "percentile" : 10.0
      }, {
        "value" : 212.37731426666667,
        "percentile" : 25.0
      }, {
        "value" : 212.37731426666667,
        "percentile" : 50.0
      }, {
        "value" : 212.37731426666667,
        "percentile" : 75.0
      }, {
        "value" : 212.37731426666667,
        "percentile" : 90.0
      } ]
    }
  } ]
}
"""

DUMMY_BENCH_RUN = JobRun(
    id="123456789",
    display_title="Test",
    html_url="https://github.com",
    run_attempt=1,
    event="push",
    head_commit=Commit(
        id="123456789",
        author=Author(name="John Doe"),
        timestamp="2021-06-01T12:00:00Z",
        message="Hello commit"
    ),
)

class TestJsonBenchResult(unittest.TestCase):
    def test_parsing_new_json_bench_report(self):
        tmpfile = tempfile.mktemp(suffix=".json")
        with open(tmpfile, "w") as f:
            f.write(SAMPLE_REPORT)
        bench_report = _parse_bench_report_from_json(tmpfile, DUMMY_BENCH_RUN)
        self.assertIsNotNone(bench_report)
        self.assertEqual(1, bench_report.results[0].samples)
        self.assertEqual(5000, bench_report.results[0].measure_millis)
        pass

    def test_new_report_is_backward_compatible(self):
        tmpfile = tempfile.mktemp(suffix=".json")
        with open(tmpfile, "w") as f:
            f.write(SAMPLE_REPORT)
        bench_report = _parse_bench_report_from_json(tmpfile, DUMMY_BENCH_RUN)
        expected_label = "org.enso.interpreter.bench.benchmarks.semantic.WarningBenchmarks.sameWarningVecSum"
        score_dict = bench_report.label_score_dict
        self.assertIsNotNone(score_dict)
        self.assertTrue(expected_label in score_dict)
        self.assertEqual(1, len(score_dict.keys()))
        self.assertEqual(212, int(score_dict[expected_label]))

        self.assertIsNotNone(bench_report.bench_run)
        self.assertEqual("push", bench_report.bench_run.event)
        self.assertEqual(1, bench_report.bench_run.run_attempt)
        self.assertEqual("John Doe", bench_report.bench_run.head_commit.author.name)
        self.assertEqual("Hello commit", bench_report.bench_run.head_commit.message)

