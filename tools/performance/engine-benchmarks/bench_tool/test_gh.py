import unittest

from bench_tool import ENSO_REPO
from .gh import ensure_gh_installed, fetch_file


# Create a unit test
class TestGH(unittest.IsolatedAsyncioTestCase):
    async def test_ensure_gh_installed(self):
        self.assertIsNone(ensure_gh_installed())

    async def test_file_fetch(self):
        content = await fetch_file(ENSO_REPO, "README.md")
        self.assertIsNotNone(content)
        self.assertIsInstance(content, str)
        self.assertGreater(len(content), 0)
