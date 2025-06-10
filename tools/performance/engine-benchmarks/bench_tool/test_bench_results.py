import json
import unittest
from datetime import datetime

from bench_tool import ENGINE_BENCH_WORKFLOW_ID, JobReport, JobRun, Commit, \
    Author
from .bench_results import get_bench_report, get_bench_runs
from .remote_cache import ReadonlyRemoteCache
from .utils import parse_commit_timestamp, WithTempDir

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
        since = datetime.fromisoformat("2024-10-01")
        until = datetime.fromisoformat("2024-10-05")
        bench_runs = await get_bench_runs(since, until, "develop", ENGINE_BENCH_WORKFLOW_ID)
        self.assertGreater(len(bench_runs), 0)
        bench_run = bench_runs[0]
        commit_ts = parse_commit_timestamp(bench_run.head_commit)
        # There are, let's say, 2 days tolerance
        self.assertLess(datetime.fromisoformat("2023-09-28"), commit_ts)
        self.assertGreater(until, commit_ts)

    async def test_get_bench_report(self):
        # We choose an old date on purpose, so that the remote cache must be used, and is thus
        # transitively tested. Note that GH deletes workflow runs that are older than 2 years.
        since = datetime.fromisoformat("2024-10-01")
        until = datetime.fromisoformat("2024-10-05")
        bench_runs = await get_bench_runs(since, until, "develop", ENGINE_BENCH_WORKFLOW_ID)
        self.assertGreater(len(bench_runs), 0)
        bench_run = bench_runs[0]
        remote_cache = ReadonlyRemoteCache()
        with WithTempDir("test_get_bench_report") as temp_dir:
            bench_report = await get_bench_report(bench_run, temp_dir, remote_cache)
            self.assertIsNotNone(bench_report)
            self.assertEqual(bench_run, bench_report.bench_run)
            self.assertEqual(70, len(bench_report.label_score_dict))
    
    async def test_get_new_bench_report(self):
        # Artifact names changed on 2025-02-03 - in PR https://github.com/enso-org/enso/pull/12226
        # This test ensures that the artifact names were correctly updated
        since = datetime.fromisoformat("2025-02-03")
        until = datetime.fromisoformat("2025-02-05")
        bench_runs = await get_bench_runs(since, until, "develop", ENGINE_BENCH_WORKFLOW_ID)
        self.assertGreater(len(bench_runs), 0)
        bench_run = bench_runs[0]
        remote_cache = ReadonlyRemoteCache()
        with WithTempDir("test_get_bench_report") as temp_dir:
            bench_report = await get_bench_report(bench_run, temp_dir, remote_cache)
            self.assertIsNotNone(bench_report)
            self.assertEqual(bench_run, bench_report.bench_run)
            self.assertEqual(80, len(bench_report.label_score_dict))


