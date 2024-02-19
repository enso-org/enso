"""
IMPORTANT NOTE: Should be run only on the CI!!

This script regenerate the benchmark results website, hosted as GH web pages on the
https://github.com/enso-org/engine-benchmark-results repo.
"""
import asyncio
import logging
from argparse import ArgumentParser
from datetime import datetime
from pathlib import Path
from typing import Optional

from bench_tool import Source, git
from bench_tool.remote_cache import SyncRemoteCache
from bench_tool.utils import generate_bench_website

# The inception date of the benchmarks, i.e., the date of the first benchmark run.
ENGINE_SINCE = datetime.fromisoformat("2022-12-01")
STDLIB_SINCE = datetime.fromisoformat("2023-08-22")
BRANCH_DEVELOP = "develop"


_logger = logging.getLogger("website_regen")


async def init_remote_cache() -> SyncRemoteCache:
    remote_cache = SyncRemoteCache()
    await remote_cache.initialize()
    return remote_cache


async def main():
    arg_parser = ArgumentParser(description="Regenerate the benchmark results website")
    arg_parser.add_argument("-v", "--verbose", action="store_true")
    arg_parser.add_argument("-n", "--dry-run", action="store_true")
    args = arg_parser.parse_args()
    verbose: bool = args.verbose
    dry_run: bool = args.dry_run
    logging.basicConfig(level=logging.DEBUG if verbose else logging.INFO)


if __name__ == "__main__":
    asyncio.run(main())
