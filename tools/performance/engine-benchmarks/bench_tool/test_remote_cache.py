import unittest

from . import JobReport, JobRun, Commit, Author
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


class TestReadonlyRemoteCache(unittest.IsolatedAsyncioTestCase):
    async def test_fetch_some_cache(self):
        remote_cache = ReadonlyRemoteCache()
        await remote_cache.initialize()
        # This ID is definitelly in the cache
        bench_id = "3686412302"
        job_report = await remote_cache.fetch(bench_id)
        self.assertIsNotNone(job_report)
        self.assertEquals(1, job_report.bench_run.run_attempt)
        self.assertEquals(bench_id, job_report.bench_run.id)
        self.assertEquals("Jaroslav Tulach", job_report.bench_run.head_commit.author.name)

    async def test_non_existing_cache_should_not_fail(self):
        remote_cache = ReadonlyRemoteCache()
        await remote_cache.initialize()
        bench_id = "FOOOO BAR"
        job_report = await remote_cache.fetch(bench_id)
        self.assertIsNone(job_report)

    async def test_put_job_report_into_cache(self):
        remote_cache = ReadonlyRemoteCache()
        await remote_cache.initialize()
        bench_id = sample_job_report.bench_run.id
        await remote_cache.put(bench_id, sample_job_report)
        job_report = await remote_cache.fetch(bench_id)
        self.assertIsNotNone(job_report)
        self.assertEquals(bench_id, job_report.bench_run.id)


# WARNING: This case can take very long
class TestSyncRemoteCache(unittest.IsolatedAsyncioTestCase):
    async def test_init_sync_remote_cache(self):
        remote_cache = SyncRemoteCache()
        await remote_cache.initialize()
        # No exception should be thrown
        self.assertTrue(True)
