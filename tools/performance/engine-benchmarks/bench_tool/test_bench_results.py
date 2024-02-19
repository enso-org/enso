import unittest
from datetime import datetime

from bench_tool import ENGINE_BENCH_WORKFLOW_ID
from .bench_results import get_bench_report, get_bench_runs
from .remote_cache import ReadonlyRemoteCache
from .utils import parse_commit_timestamp, WithTempDir

# A single ID for a benchmark run between 2023-05-01 and 2023-05-05
# We know for sure that this workflow run is on the GH.
BENCH_RUN_ID = "4888453297"


class MyTestCase(unittest.IsolatedAsyncioTestCase):
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
            self.assertEquals(bench_run, bench_report.bench_run)
            self.assertEquals(55, len(bench_report.label_score_dict))

