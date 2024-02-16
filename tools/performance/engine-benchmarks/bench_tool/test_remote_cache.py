import unittest

from .remote_cache import ReadonlyRemoteCache


class TestGH(unittest.IsolatedAsyncioTestCase):
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
