import shutil
import tempfile
import unittest
from pathlib import Path

from . import git


class TestGit(unittest.IsolatedAsyncioTestCase):
    def setUp(self):
        self.repo_root = Path(tempfile.mkdtemp())

    def tearDown(self):
        shutil.rmtree(self.repo_root)

    async def test_init(self):
        await git.init(self.repo_root)
        status = await git.status(self.repo_root)
        self.assertEqual(0, len(status.added))
        self.assertEqual(0, len(status.modified))
        self.assertEqual(0, len(status.untracked))

    async def test_add_file(self):
        await git.init(self.repo_root)
        self.repo_root.joinpath("README.md").write_text("Hello")
        status = await git.status(self.repo_root)
        self.assertEqual(1, len(status.untracked))

    async def test_commit(self):
        await git.init(self.repo_root)
        self.repo_root.joinpath("README.md").write_text("Hello")
        await git.add(self.repo_root, {"README.md"})
        await git.commit(self.repo_root, "Initial commit")
        status = await git.status(self.repo_root)
        self.assertEqual(0, len(status.added))
        self.assertEqual(0, len(status.modified))
        self.assertEqual(0, len(status.untracked))

    async def test_modify_file(self):
        await git.init(self.repo_root)
        self.repo_root.joinpath("README.md").write_text("Hello")
        await git.add(self.repo_root, {"README.md"})
        await git.commit(self.repo_root, "Initial commit")
        self.repo_root.joinpath("README.md").write_text("Hello World")
        status = await git.status(self.repo_root)
        self.assertEqual(0, len(status.added))
        self.assertEqual(1, len(status.modified))
        self.assertEqual(0, len(status.untracked))

    async def test_add_more_files(self):
        await git.init(self.repo_root)
        self.repo_root.joinpath("README.md").write_text("Hello")
        self.repo_root.joinpath("pom.xml").write_text("<xml></xml>")
        status = await git.status(self.repo_root)
        self.assertEqual(2, len(status.untracked))
        await git.add(self.repo_root, {"README.md", "pom.xml"})
        status = await git.status(self.repo_root)
        self.assertEqual(2, len(status.added))

