import json
import unittest
from datetime import datetime

from bench_tool import ENGINE_BENCH_WORKFLOW_ID, JobReport, JobRun, Commit, \
    Author
from .bench_results import get_bench_report, get_bench_runs
from .remote_cache import ReadonlyRemoteCache
from .utils import parse_commit_timestamp, WithTempDir

# A single ID for a benchmark run between 2023-05-01 and 2023-05-05
# We know for sure that this workflow run is on the GH.
BENCH_RUN_ID = "4888453297"

sample_job_report = JobReport(
    label_score_dict={
        "test_label": 1.0
    },
    bench_run=JobRun(
        id="123456789",
        display_title="Test",
        html_url="https://github.com/enso-org/enso/actions/runs/123456789",
        run_attempt=1,
        event="push",
        head_commit=Commit(
            id="a67297aebf6a094d1ad0b0d88cf7438dbf8bd8fe",
            message="Test commit",
            timestamp="2021-06-01T12:00:00Z",
            author=Author(
                name="Pavel Marek"
            )
        )
    )
)


class TestBenchResults(unittest.IsolatedAsyncioTestCase):
    def test_job_report_is_serializable(self):
        s = json.dumps(sample_job_report.to_dict())
        self.assertIsNotNone(s)
        self.assertGreater(len(s), 0)

    def test_job_report_is_deserializable(self):
        d = sample_job_report.to_dict()
        job_report = JobReport.from_dict(d)
        self.assertEqual(sample_job_report, job_report)

    async def test_get_bench_run(self):
        """
        Bench run does not need remote cache - it fetches just some metadata about GH artifacts.
        :return:
        """
        since = datetime.fromisoformat("2023-05-01")
        until = datetime.fromisoformat("2023-05-05")
        bench_runs = await get_bench_runs(since, until, "develop", ENGINE_BENCH_WORKFLOW_ID)
        self.assertEqual(1, len(bench_runs))
        # There is just a single bench run between 2023-05-01 and 2023-05-05
        bench_run = bench_runs[0]
        self.assertEqual(BENCH_RUN_ID, bench_run.id)
        commit_ts = parse_commit_timestamp(bench_run.head_commit)
        self.assertLess(since, commit_ts)
        self.assertGreater(until, commit_ts)

    async def test_get_bench_report(self):
        # We choose an old date on purpose, so that the remote cache must be used, and is thus
        # transitively tested.
        since = datetime.fromisoformat("2023-05-01")
        until = datetime.fromisoformat("2023-05-05")
        bench_runs = await get_bench_runs(since, until, "develop", ENGINE_BENCH_WORKFLOW_ID)
        self.assertEqual(1, len(bench_runs))
        bench_run = bench_runs[0]
        remote_cache = ReadonlyRemoteCache()
        with WithTempDir("test_get_bench_report") as temp_dir:
            bench_report = await get_bench_report(bench_run, temp_dir, remote_cache)
            self.assertIsNotNone(bench_report)
            self.assertEqual(bench_run, bench_report.bench_run)
            self.assertEqual(55, len(bench_report.label_score_dict))

