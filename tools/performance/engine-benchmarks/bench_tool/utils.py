import logging
import shutil
import tempfile
from datetime import datetime, timedelta
from pathlib import Path
from typing import List, Set, Dict

from bench_tool import JobReport, GH_DATE_FORMAT, Commit, Source, JobRun, TemplateBenchData, JinjaData, BRANCH_DEVELOP
from bench_tool.bench_results import get_bench_runs, fetch_job_reports
from bench_tool.remote_cache import SyncRemoteCache
from bench_tool.template_render import create_template_data, render_html

_logger = logging.getLogger(__name__)


class WithTempDir:
    def __init__(self, prefix: str):
        self.prefix = prefix
        self.temp_dir = None

    def __enter__(self):
        self.temp_dir = tempfile.mkdtemp(prefix=self.prefix)
        return self.temp_dir

    def __exit__(self, exc_type, exc_val, exc_tb):
        shutil.rmtree(self.temp_dir, ignore_errors=True)


def gather_all_bench_labels(job_reports: List[JobReport]) -> Set[str]:
    """
    Iterates through all the job reports and gathers all the benchmark labels
    found. Note that every job report can have a different set of benchmark labels.
    :return: List of benchmark labels.
    """
    all_labels = set()
    for job_report in job_reports:
        for labels in job_report.label_score_dict.keys():
            all_labels.add(labels)
    return all_labels


def parse_commit_timestamp(commit: Commit) -> datetime:
    """ Parses the timestamp from the commit based on the GH's formatting. """
    return datetime.strptime(commit.timestamp, GH_DATE_FORMAT)


def sort_job_reports(
        job_reports: List[JobReport]
) -> None:
    """
    Sorts the job reports in place by the commit date.
    :param job_reports:
    :return:
    """
    def _get_timestamp(job_report: JobReport) -> datetime:
        return parse_commit_timestamp(job_report.bench_run.head_commit)

    job_reports.sort(key=lambda report: _get_timestamp(report))


async def generate_bench_website(
        bench_source: Source,
        remote_cache: SyncRemoteCache,
        since: datetime,
        until: datetime,
        generated_html: Path
) -> None:
    """
    Generates single `index.html` website with the benchmark results.

    :param bench_source: Source of the benchmarks, either engine or stdlib
    :param remote_cache: Remote cache used for fetching the job reports.
    :param since: Date since when the benchmarks should be considered
    :param until: Date until when the benchmarks should be considered
    :param generated_html: Path to the generated HTML file
    :return:
    """
    bench_runs: List[JobRun] = []
    for workflow_id in bench_source.workflow_ids():
        bench_runs.extend(
            await get_bench_runs(since, until, BRANCH_DEVELOP, workflow_id)
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
        display_since=max(until - timedelta(days=30), since),
        until=until,
        bench_datas=template_bench_datas,
        bench_source=bench_source,
        branches=[BRANCH_DEVELOP],
    )
    _logger.debug(f"Rendering HTML to {generated_html}")
    render_html(jinja_data, generated_html)
