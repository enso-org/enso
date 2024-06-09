import unittest
from pathlib import Path
from datetime import datetime

from bench_tool import Source
from bench_tool.remote_cache import SyncRemoteCache
from bench_tool.utils import WithTempDir
from bench_tool.website import generate_bench_website


class TestWebsiteRegen(unittest.IsolatedAsyncioTestCase):
    LOCAL_REPO_ROOT = Path("/home/pavel/dev/engine-benchmark-results")

    async def test_engine_website_regen(self):
        if not self.LOCAL_REPO_ROOT.exists():
            self.skipTest(f"Local repo {self.LOCAL_REPO_ROOT} does not exist")
        remote_cache = SyncRemoteCache(self.LOCAL_REPO_ROOT)
        # Pull the repo if necessary
        await remote_cache.initialize()
        since = datetime.fromisoformat("2023-02-01")
        until = datetime.fromisoformat("2023-02-25")
        with WithTempDir("test_engine_website_regen") as temp_dir:
            temp_dir_path = Path(temp_dir)
            html_out = temp_dir_path.joinpath("engine-benchs.html")
            await generate_bench_website(Source.ENGINE, remote_cache, since, until, html_out)
            self.assertTrue(html_out.exists())
            self.assertGreater(
                html_out.stat().st_size, 100 * 1024,
                "The generated HTML file should have size bigger than 100 KB"
            )
        pass
