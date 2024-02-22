import unittest
from pathlib import Path

from . import JobReport, JobRun, Commit, Author
from .bench_results import fetch_job_reports
from .remote_cache import ReadonlyRemoteCache, SyncRemoteCache


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

stdlib_bench_run = JobRun(
    id='7879611014',
    display_title='Benchmark Standard Libraries',
    html_url='https://github.com/enso-org/enso/actions/runs/7879611014',
    run_attempt=1,
    event='schedule',
    head_commit=Commit(
        id='eb59b475f68146f03fc3cef1092ee56eaaa1600a',
        author=Author(name='Radosław Waśko'),
        timestamp='2024-02-12T19:04:13Z',
        message='Write support for S3 (#8921)\n\n- Closes #8809'
    )
)


class TestReadonlyRemoteCache(unittest.IsolatedAsyncioTestCase):
    async def test_fetch_some_cache(self):
        remote_cache = ReadonlyRemoteCache()
        # This ID is definitelly in the cache
        bench_id = "3686412302"
        job_report = await remote_cache.fetch(bench_id)
        self.assertIsNotNone(job_report)
        self.assertEqual(1, job_report.bench_run.run_attempt)
        self.assertEqual(bench_id, job_report.bench_run.id)
        self.assertEqual("Jaroslav Tulach", job_report.bench_run.head_commit.author.name)

    async def test_non_existing_cache_should_not_fail(self):
        remote_cache = ReadonlyRemoteCache()
        bench_id = "FOOOO BAR"
        job_report = await remote_cache.fetch(bench_id)
        self.assertIsNone(job_report)

    async def test_put_job_report_into_cache(self):
        remote_cache = ReadonlyRemoteCache()
        bench_id = sample_job_report.bench_run.id
        await remote_cache.put(bench_id, sample_job_report)
        job_report = await remote_cache.fetch(bench_id)
        self.assertIsNotNone(job_report)
        self.assertEqual(bench_id, job_report.bench_run.id)

    async def test_fetch_stdlib_report(self):
        remote_cache = ReadonlyRemoteCache()
        job_reports = await fetch_job_reports([stdlib_bench_run], remote_cache)
        self.assertIsNotNone(job_reports)
        self.assertEqual(1, len(job_reports))


class TestSyncRemoteCache(unittest.IsolatedAsyncioTestCase):
    LOCAL_REPO_ROOT = Path("/home/pavel/dev/engine-benchmark-results")

    async def test_init_sync_remote_cache_from_local_repo(self):
        if not self.LOCAL_REPO_ROOT.exists():
            self.skipTest(f"Local repo {self.LOCAL_REPO_ROOT} does not exist")
        remote_cache = SyncRemoteCache(self.LOCAL_REPO_ROOT)
        await remote_cache.initialize()
        root_dir = remote_cache.repo_root_dir()
        self.assertTrue(root_dir.exists())
        self.assertTrue(root_dir.is_dir())
        cache_dir = remote_cache.cache_dir()
        self.assertTrue(cache_dir.exists())
        self.assertTrue(cache_dir.is_dir())
        self.assertTrue(remote_cache.engine_index_html().exists())
        self.assertTrue(remote_cache.stdlib_index_html().exists())

    async def test_clone_sync_remote_cache(self):
        self.skipTest("TODO: Takes too long")
        remote_cache = SyncRemoteCache()
        await remote_cache.initialize()
        root_dir = remote_cache.repo_root_dir()
        self.assertTrue(root_dir.exists())
        self.assertTrue(root_dir.is_dir())
        cache_dir = remote_cache.cache_dir()
        self.assertTrue(cache_dir.exists())
        self.assertTrue(cache_dir.is_dir())
        self.assertTrue(remote_cache.engine_index_html().exists())
        self.assertTrue(remote_cache.stdlib_index_html().exists())

    async def test_fetch_stdlib_report(self):
        if not self.LOCAL_REPO_ROOT.exists():
            self.skipTest(f"Local repo {self.LOCAL_REPO_ROOT} does not exist")
        remote_cache = SyncRemoteCache(self.LOCAL_REPO_ROOT)
        await remote_cache.initialize()
        job_reports = await fetch_job_reports([stdlib_bench_run], remote_cache)
        self.assertIsNotNone(job_reports)
        self.assertEqual(1, len(job_reports))
