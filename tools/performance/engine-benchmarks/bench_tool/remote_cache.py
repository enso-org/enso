"""
A remote cache is located inhttps://github.com/enso-org/engine-benchmark-results/tree/main/cache.
It is just a bunch of JSON files, each representing a single job report.
"""
import abc
import json
import logging
import os
import re
import tempfile
from pathlib import Path
from typing import Dict, Optional

from . import gh, JobReport, BENCH_REPO, git

_logger = logging.getLogger(__name__)

CACHE_REMOTE_DIR = "cache"
ENGINE_INDEX_HTML = "engine-benchs.html"
STDLIB_INDEX_HTML = "stdlib-benchs.html"


class RemoteCache(abc.ABC):

    @abc.abstractmethod
    async def fetch(self, bench_id: str) -> Optional[JobReport]:
        """
        Fetches a job report for the given bench ID from the remote cache
        :param bench_id:
        :return: None if the report does not exist
        """
        raise NotImplementedError

    @abc.abstractmethod
    async def put(self, bench_id: str, job_report: JobReport) -> None:
        """
        Puts a job report to the remote cache, or to the internal data structures.
        :param bench_id:
        :param job_report:
        :return:
        """
        raise NotImplementedError


class ReadonlyRemoteCache(RemoteCache):
    """
    Only fetches the artifacts from the remote cache, does not push anything.
    """

    def __init__(self):
        self._fetched_items: Dict[str, JobReport] = {}

    async def fetch(self, bench_id: str) -> Optional[JobReport]:
        """ Fetches a job report for the given bench ID from the remote cache """
        if bench_id in self._fetched_items:
            return self._fetched_items[bench_id]
        if not _is_benchrun_id(bench_id):
            _logger.warning("Invalid bench ID: %s", bench_id)
            return None
        remote_path = self._get_remote_path(bench_id)
        _logger.debug("Fetching cache from %s", remote_path)
        content = await gh.fetch_file(BENCH_REPO, remote_path)
        if content is None:
            _logger.warning("Cache not found for %s", bench_id)
            return None
        bench_report = JobReport.from_dict(
            json.loads(content)
        )
        assert bench_id not in self._fetched_items
        self._fetched_items[bench_id] = bench_report
        return bench_report

    async def put(self, bench_id: str, job_report: JobReport) -> None:
        assert _is_benchrun_id(bench_id)
        assert bench_id not in self._fetched_items
        self._fetched_items[bench_id] = job_report

    def _get_remote_path(self, bench_id: str) -> str:
        assert _is_benchrun_id(bench_id)
        return os.path.join(CACHE_REMOTE_DIR, bench_id + ".json")


class SyncRemoteCache(RemoteCache):
    """
    Fetches and pushes the artifacts to the remote cache. Needs a write permissions to the repo.
    """

    def __init__(self, local_root_dir: Optional[Path] = None):
        if local_root_dir is not None:
            assert local_root_dir.exists()
            assert local_root_dir.is_dir()
            assert local_root_dir.joinpath(".git").exists()
            self._repo_root_dir = local_root_dir
            self._should_clone = False
        else:
            self._repo_root_dir = Path(tempfile.mkdtemp(prefix="bench_tool_remote_cache"))
            self._should_clone = True
        assert self._repo_root_dir.exists()
        assert self._repo_root_dir.is_dir()
        self._cache_dir = self._repo_root_dir.joinpath(CACHE_REMOTE_DIR)

    def repo_root_dir(self) -> Path:
        return self._repo_root_dir

    def cache_dir(self) -> Path:
        return self._cache_dir

    def engine_index_html(self) -> Path:
        return self._repo_root_dir.joinpath(ENGINE_INDEX_HTML)

    def stdlib_index_html(self) -> Path:
        return self._repo_root_dir.joinpath(STDLIB_INDEX_HTML)

    async def initialize(self) -> None:
        """
        Make sure the repo is up-to-date
        :return:
        """
        if self._should_clone:
            await git.clone(BENCH_REPO, self._repo_root_dir)
        else:
            await git.pull(self._repo_root_dir)
        assert self._repo_root_dir.exists()
        assert self._cache_dir.exists()

    async def fetch(self, bench_id: str) -> Optional[JobReport]:
        assert self._cache_dir.exists()
        path = self._cache_dir.joinpath(bench_id + ".json")
        if path.exists():
            with path.open() as f:
                return JobReport.from_dict(json.load(f))
        return None

    async def put(self, bench_id: str, job_report: JobReport) -> None:
        assert self._cache_dir.exists()
        path = self._cache_dir.joinpath(bench_id + ".json")
        assert not path.exists()
        with path.open("w") as f:
            json.dump(
                job_report.to_dict(),
                f,
                ensure_ascii=True,
                indent=2
            )

    async def sync(self) -> None:
        """
        Synchronizes the local repo state with upstream. That means, pushes if some untracked or
        modified files are in the local directory.
        :return:
        """
        status = await git.status(self._repo_root_dir)
        is_repo_dirty = len(status.modified) > 0 or len(status.added) > 0
        if is_repo_dirty:
            _logger.info("Untracked or modified files found in the repo: %s", self._repo_root_dir)
            if len(status.modified) > 0:
                _logger.debug("Modified files: %s", status.modified)
                await git.add(self._repo_root_dir, status.modified)
            if len(status.untracked) > 0:
                _logger.debug("Untracked files: %s", status.untracked)
                await git.add(self._repo_root_dir, status.untracked)
            await git.commit(self._repo_root_dir, f"Add {len(status.untracked)} new reports")
            await git.push(self._repo_root_dir)


def _is_benchrun_id(name: str) -> bool:
    return re.match(r"\d{9}", name) is not None



