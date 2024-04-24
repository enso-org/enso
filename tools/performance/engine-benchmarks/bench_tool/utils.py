import logging
import shutil
import tempfile
from datetime import datetime
from typing import List, Set

from bench_tool import JobReport, GH_DATE_FORMAT, Commit

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
