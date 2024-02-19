"""
A remote cache is located inhttps://github.com/enso-org/engine-benchmark-results/tree/main/cache.
It is just a bunch of JSON files, each representing a single job report.
"""
import abc
import json
import logging
import os
import re
import shutil
import tempfile
from pathlib import Path
from typing import Dict, Optional, Any

from . import gh, JobReport, BENCH_REPO, JobRun, Commit, Author, git

_logger = logging.getLogger(__name__)

CACHE_REMOTE_DIR = "cache"


class RemoteCache(abc.ABC):
    @abc.abstractmethod
    async def initialize(self) -> None:
        """
        Initializes the remote cache.
        :return:
        """
        raise NotImplementedError

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

    @abc.abstractmethod
    async def sync(self) -> None:
        """
        Synchronizes the remote cache with the local state.
        :return:
        """
        raise NotImplementedError


class ReadonlyRemoteCache(RemoteCache):
    """
    Only fetches the artifacts from the remote cache, does not push anything.
    """

    def __init__(self):
        self._fetched_items: Dict[str, JobReport] = {}

    async def initialize(self) -> None:
        # Nop
        pass

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
        bench_report = _parse_bench_report_from_json(
            json.loads(content)
        )
        assert bench_id not in self._fetched_items
        self._fetched_items[bench_id] = bench_report
        return bench_report

    async def put(self, bench_id: str, job_report: JobReport) -> None:
        assert _is_benchrun_id(bench_id)
        assert bench_id not in self._fetched_items
        self._fetched_items[bench_id] = job_report

    async def sync(self) -> None:
        # Nop
        pass

    def _get_remote_path(self, bench_id: str) -> str:
        assert _is_benchrun_id(bench_id)
        return os.path.join(CACHE_REMOTE_DIR, bench_id + ".json")


class SyncRemoteCache(RemoteCache):
    """
    Fetches and pushes the artifacts to the remote cache. Needs a write permissions to the repo.
    """

    def __init__(self):
        self._repo_root_dir = Path(tempfile.mkdtemp(prefix="bench_tool_remote_cache"))
        self._cache_dir = self._repo_root_dir.joinpath(CACHE_REMOTE_DIR)

    def repo_root_dir(self) -> Path:
        return self._repo_root_dir

    async def initialize(self) -> None:
        # Checkout the repo
        await git.clone(BENCH_REPO, self._repo_root_dir)
        assert self._repo_root_dir.exists()
        assert self._cache_dir.exists()

    async def fetch(self, bench_id: str) -> Optional[JobReport]:
        assert self._cache_dir.exists()
        path = self._cache_dir.joinpath(bench_id + ".json")
        if path.exists():
            with path.open() as f:
                return _parse_bench_report_from_json(json.load(f))
        return None

    async def put(self, bench_id: str, job_report: JobReport) -> None:
        assert self._cache_dir.exists()
        path = self._cache_dir.joinpath(bench_id + ".json")
        assert not path.exists()
        with path.open("w") as f:
            json.dump(job_report, f)

    async def sync(self) -> None:
        status = await git.status(self._repo_root_dir)
        assert len(status.modified) == 0, "The RemoteCache should not modify any files, only add new ones"
        assert len(status.added) == 0, f"Only untracked files expected in {self._repo_root_dir}"
        if len(status.untracked) > 0:
            _logger.info("Untracked files found in the remote cache: %s", status.untracked)
            await git.add(self._repo_root_dir, status.untracked)
            await git.commit(self._repo_root_dir, f"Add {len(status.untracked)} new reports")
            await git.push(self._repo_root_dir)
        shutil.rmtree(self._repo_root_dir, ignore_errors=True)


def _is_benchrun_id(name: str) -> bool:
    return re.match(r"\d{9}", name) is not None


def _parse_bench_report_from_json(obj: Dict[Any, Any]) -> JobReport:
    return JobReport(
        bench_run=_parse_bench_run_from_json(obj["bench_run"]),
        label_score_dict=obj["label_score_dict"]
    )


def _parse_bench_run_from_json(obj: Dict[Any, Any]) -> JobRun:
    return JobRun(
        id=str(obj["id"]),
        html_url=obj["html_url"],
        run_attempt=int(obj["run_attempt"]),
        event=obj["event"],
        display_title=obj["display_title"],
        head_commit=Commit(
            id=obj["head_commit"]["id"],
            message=obj["head_commit"]["message"],
            timestamp=obj["head_commit"]["timestamp"],
            author=Author(
                name=obj["head_commit"]["author"]["name"]
            )
        )
    )
