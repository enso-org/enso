import asyncio
import logging
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import Set

_logger = logging.getLogger(__name__)


@dataclass
class GitStatus:
    modified: Set[str]
    untracked: Set[str]
    added: Set[str]


async def clone(repo: str, dest: Path) -> None:
    _logger.debug("Cloning %s to %s", repo, dest)
    dest_abs_path = str(dest.absolute())
    args = ["clone", f"git@github.com:{repo}.git", dest_abs_path]
    proc = await asyncio.create_subprocess_exec("git", *args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    ret = await proc.wait()
    if ret != 0:
        stdout, stderr = await proc.communicate()
        out = stdout.decode() + stderr.decode()
        raise RuntimeError(f"Failed to clone {repo}: {out}")
    assert dest.exists()


async def status(repo: Path) -> GitStatus:
    assert repo.exists()
    proc = await asyncio.create_subprocess_exec("git", "status", "--porcelain", cwd=repo,
                                                stdout=subprocess.PIPE)
    out, _ = await proc.communicate()
    lines = out.decode().splitlines()
    untracked: Set[str] = set()
    modified: Set[str] = set()
    added: Set[str] = set()
    for line in lines:
        if line.startswith("??"):
            untracked.add(line.split()[1])
        elif line.startswith(" M"):
            modified.add(line.split()[1])
        elif line.startswith("A "):
            added.add(line.split()[1])
    return GitStatus(modified, untracked, added)


async def add(repo: Path, files: Set[str]) -> None:
    args = ["add"] + list(files)
    proc = await asyncio.create_subprocess_exec("git", *args, cwd=repo, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    ret = await proc.wait()
    if ret != 0:
        raise RuntimeError(f"Failed to add {files} to {repo}")


async def commit(repo: Path, msg: str) -> None:
    _logger.debug("Committing %s with message %s", repo, msg)
    stat = await status(repo)
    assert len(stat.added) > 0
    args = ["commit", "-m", msg]
    proc = await asyncio.create_subprocess_exec("git", *args, cwd=repo, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    ret = await proc.wait()
    if ret != 0:
        raise RuntimeError(f"Failed to commit {repo}")


async def push(repo: Path) -> None:
    _logger.debug("Pushing to %s", repo)
    args = ["push"]
    proc = await asyncio.create_subprocess_exec("git", *args, cwd=repo, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    ret = await proc.wait()
    if ret != 0:
        raise RuntimeError(f"Failed to push {repo}")


async def init(repo: Path) -> None:
    _logger.debug("Initializing git repo in %s", repo)
    assert repo.exists()
    args = ["init"]
    proc = await asyncio.create_subprocess_exec("git", *args, cwd=repo, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    ret = await proc.wait()
    if ret != 0:
        raise RuntimeError(f"Failed to init {repo}")
