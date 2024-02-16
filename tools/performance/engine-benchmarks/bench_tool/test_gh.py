import unittest

from bench_tool import ENSO_REPO
from .gh import ensure_gh_installed, fetch_file, invoke_gh_api


# Create a unit test
class TestGH(unittest.IsolatedAsyncioTestCase):
    async def test_ensure_gh_installed(self):
        self.assertIsNone(ensure_gh_installed())

    async def test_file_fetch(self):
        content = await fetch_file(ENSO_REPO, "README.md")
        self.assertIsNotNone(content)
        self.assertIsInstance(content, str)
        self.assertGreater(len(content), 0)

    async def test_wrong_gh_query_should_not_fail(self):
        res = await invoke_gh_api("non_existing_repo", "/non_existing_endpoint")
        self.assertIsNone(res)
