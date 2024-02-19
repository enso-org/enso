"""
IMPORTANT NOTE: Should be run only on the CI!!

This script regenerate the benchmark results website, hosted as GH web pages on the
https://github.com/enso-org/engine-benchmark-results repo.
"""
import logging
from argparse import ArgumentParser
from datetime import datetime, timedelta
from pathlib import Path
from typing import List, Dict, Set

from bench_tool import Source, JobRun, JobReport, TemplateBenchData, JinjaData
from bench_tool.bench_results import get_bench_runs, fetch_job_reports
from bench_tool.remote_cache import SyncRemoteCache
from bench_tool.template_render import create_template_data, render_html
from bench_tool.utils import gather_all_bench_labels, sort_job_reports

# The inception date of the benchmarks, i.e., the date of the first benchmark run.
ENGINE_SINCE = datetime.fromisoformat("2022-12-01")
STDLIB_SINCE = datetime.fromisoformat("2023-08-22")
BRANCH_DEVELOP = "develop"


_logger = logging.getLogger("website_regen")


async def init_remote_cache() -> SyncRemoteCache:
    remote_cache = SyncRemoteCache()
    await remote_cache.initialize()
    return remote_cache


async def generate_bench_website(
        bench_source: Source,
        remote_cache: SyncRemoteCache,
        since: datetime,
        generated_html: Path
) -> None:
    """
    Generates single `index.html` website with the benchmark results.

    :param bench_source: Source of the benchmarks, either engine or stdlib
    :param remote_cache: Remote cache used for fetching the job reports.
    :param since: Date since when the benchmarks should be considered
    :param generated_html: Path to the generated HTML file
    :return:
    """
    bench_runs: List[JobRun] = []
    now = datetime.now()
    for workflow_id in bench_source.workflow_ids():
        bench_runs.extend(
            await get_bench_runs(since, now, BRANCH_DEVELOP, workflow_id)
        )
    assert len(bench_runs) > 0, "No benchmark runs found"

    job_reports = await fetch_job_reports(bench_runs, remote_cache)
    _logger.debug(f"Gathered {len(job_reports)} job reports")
    assert len(job_reports) > 0, "No job reports found"

    _logger.debug("Sorting job_reports by commit date")
    sort_job_reports(job_reports)

    all_bench_labels: Set[str] = gather_all_bench_labels(job_reports)
    _logger.debug(f"Found {len(all_bench_labels)} unique benchmark labels")

    job_reports_per_branch: Dict[str, List[JobReport]] = {
        BRANCH_DEVELOP: job_reports
    }
    template_bench_datas: List[TemplateBenchData] =\
        create_template_data(job_reports_per_branch, all_bench_labels)
    template_bench_datas.sort(key=lambda data: data.id)

    jinja_data = JinjaData(
        since=since,
        display_since=max(now - timedelta(days=30), since),
        until=now,
        bench_datas=template_bench_datas,
        bench_source=bench_source,
        branches=[BRANCH_DEVELOP],
    )
    _logger.debug(f"Rendering HTML to {generated_html}")
    render_html(jinja_data, generated_html)
    pass


if __name__ == '__main__':
    arg_parser = ArgumentParser(description="Regenerate the benchmark results website")
    arg_parser.add_argument("-v", "--verbose", action="store_true")
    arg_parser.add_argument("-n", "--dry-run", action="store_true")
    args = arg_parser.parse_args()
    verbose: bool = args.verbose
    dry_run: bool = args.dry_run
    logging.basicConfig(level=logging.DEBUG if verbose else logging.INFO)

