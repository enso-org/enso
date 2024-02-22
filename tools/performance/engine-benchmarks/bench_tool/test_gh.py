import unittest

from bench_tool import ENSO_REPO, Source
from . import gh


class TestGH(unittest.IsolatedAsyncioTestCase):
    async def test_ensure_gh_installed(self):
        self.assertIsNone(gh.ensure_gh_installed())

    async def test_file_fetch(self):
        content = await gh.fetch_file(ENSO_REPO, "README.md")
        self.assertIsNotNone(content)
        self.assertIsInstance(content, str)
        self.assertGreater(len(content), 0)

    async def test_fetch_non_existing_file(self):
        content = await gh.fetch_file(ENSO_REPO, "non_existing_file")
        self.assertIsNone(content)

    async def test_wrong_gh_query_should_not_fail(self):
        res = await gh.invoke_gh_api("non_existing_repo", "/non_existing_endpoint")
        self.assertIsNone(res)

    async def test_get_stdlib_bench_run(self):
        # This bench run ID does not contain the "Runtime Benchmark Report" artifact name,
        # but it is a successful run. There should be a special handling for this case
        # https://github.com/enso-org/enso/actions/runs/7909011591
        bench_run_id = "7909011591"
        obj = await gh.invoke_gh_api(ENSO_REPO, f"/actions/runs/{bench_run_id}/artifacts")
        artifacts = obj["artifacts"]
        stdlib_artifact_name = Source.STDLIB.artifact_names()[0]
        self.assertEqual(1, len(artifacts))
        self.assertEqual(stdlib_artifact_name, artifacts[0]["name"])
