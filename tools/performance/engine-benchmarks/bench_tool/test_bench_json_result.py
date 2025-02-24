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
  "$schema" : "https://raw.githubusercontent.com/enso-org/enso/6732a5e7e94ad3395c49627fa2d3417d4a7fcd68/lib/java/benchmarks-common/src/main/resources/results_schema.json",
  "configuration" : {
    "osName" : "Linux",
    "osArch" : "amd64",
    "osVersion" : "6.8.0-53-generic",
    "vmName" : "OpenJDK 64-Bit Server VM",
    "vmVersion" : "21.0.2+13-jvmci-23.1-b30",
    "vmVendor" : "GraalVM Community",
    "jdkVersion" : "21.0.2"
  },
  "results" : [ {
    "label" : "org.enso.benchmarks.generated.Vector_Operations.Sum",
    "timestamp" : "2025-02-24T12:26:58Z",
    "score" : 0.9694779908879411,
    "samples" : 1,
    "warmupIterations" : 1,
    "warmupMillis" : 5000,
    "measureIterations" : 1,
    "measureMillis" : 5000,
    "commitId" : "cd7a3cec11d4105afb106b7b1c781e3bdedf4236",
    "branch" : "wip/akirathan/9355-bench-res-json",
    "measurementStatistics" : {
      "stddev" : "NaN",
      "mean" : 0.9694779908879411,
      "min" : 0.9694779908879411,
      "max" : 0.9694779908879411,
      "error50" : "NaN",
      "error95" : "NaN",
      "percentiles" : [ {
        "value" : 0.9694779908879411,
        "percentile" : 10.0
      }, {
        "value" : 0.9694779908879411,
        "percentile" : 25.0
      }, {
        "value" : 0.9694779908879411,
        "percentile" : 50.0
      }, {
        "value" : 0.9694779908879411,
        "percentile" : 75.0
      }, {
        "value" : 0.9694779908879411,
        "percentile" : 90.0
      } ]
    }
  }, {
    "label" : "org.enso.benchmarks.generated.Vector_Operations.Sum_Stats",
    "timestamp" : "2025-02-24T12:26:58Z",
    "score" : 392.2233642307692,
    "samples" : 1,
    "warmupIterations" : 1,
    "warmupMillis" : 5000,
    "measureIterations" : 1,
    "measureMillis" : 5000,
    "commitId" : "cd7a3cec11d4105afb106b7b1c781e3bdedf4236",
    "branch" : "wip/akirathan/9355-bench-res-json",
    "measurementStatistics" : {
      "stddev" : "NaN",
      "mean" : 392.2233642307692,
      "min" : 392.2233642307692,
      "max" : 392.2233642307692,
      "error50" : "NaN",
      "error95" : "NaN",
      "percentiles" : [ {
        "value" : 392.2233642307692,
        "percentile" : 10.0
      }, {
        "value" : 392.2233642307692,
        "percentile" : 25.0
      }, {
        "value" : 392.2233642307692,
        "percentile" : 50.0
      }, {
        "value" : 392.2233642307692,
        "percentile" : 75.0
      }, {
        "value" : 392.2233642307692,
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
        expected_label = "org.enso.benchmarks.generated.Vector_Operations.Sum_Stats"
        score_dict = bench_report.label_score_dict
        self.assertIsNotNone(score_dict)
        self.assertTrue(expected_label in score_dict)
        self.assertEqual(2, len(score_dict.keys()))
        self.assertEqual(392, int(score_dict[expected_label]))

        self.assertIsNotNone(bench_report.bench_run)
        self.assertEqual("push", bench_report.bench_run.event)
        self.assertEqual(1, bench_report.bench_run.run_attempt)
        self.assertEqual("John Doe", bench_report.bench_run.head_commit.author.name)
        self.assertEqual("Hello commit", bench_report.bench_run.head_commit.message)

