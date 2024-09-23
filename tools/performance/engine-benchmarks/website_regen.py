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

from bench_tool import Source
from bench_tool.remote_cache import SyncRemoteCache
from bench_tool.website import generate_bench_website

# The inception date of the benchmarks, i.e., the date of the first benchmark run.
ENGINE_SINCE = datetime.fromisoformat("2024-04-01")
STDLIB_SINCE = datetime.fromisoformat("2024-04-01")

_logger = logging.getLogger("website_regen")


async def main():
    arg_parser = ArgumentParser(description="Regenerate the benchmark results website")
    arg_parser.add_argument("-v", "--verbose", action="store_true")
    arg_parser.add_argument("-n", "--dry-run", action="store_true")
    arg_parser.add_argument("--local-repo",
                            type=str,
                            help="Path to the local clone of the engine-benchmark-results repo")
    args = arg_parser.parse_args()
    dry_run: bool = args.dry_run
    verbose: bool = args.verbose
    local_repo: Optional[Path] = Path(args.local_repo) if args.local_repo else None
    logging.basicConfig(level=logging.DEBUG if verbose else logging.INFO)
    _logger.debug(f"Args: dry_run={dry_run}, verbose={verbose}, local_repo={local_repo}")
    remote_cache = SyncRemoteCache(local_repo)
    _logger.info("Initializing the bench results repo, this might take some time")
    await remote_cache.initialize()
    _logger.info("Bench results repo initialized")

    now = datetime.now()
    engine_html_task = generate_bench_website(
        Source.ENGINE,
        remote_cache,
        ENGINE_SINCE,
        now,
        remote_cache.engine_index_html()
    )
    stdlib_html_task = generate_bench_website(
        Source.STDLIB,
        remote_cache,
        STDLIB_SINCE,
        now,
        remote_cache.stdlib_index_html()
    )
    await asyncio.gather(engine_html_task, stdlib_html_task)
    if dry_run:
        _logger.info("Dry-run, not syncing the remote cache")
    else:
        await remote_cache.sync()


if __name__ == "__main__":
    asyncio.run(main())
