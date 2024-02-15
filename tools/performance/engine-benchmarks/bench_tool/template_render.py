import logging
from datetime import datetime
from os import path
from typing import List, Dict, Optional, Set

import jinja2
import numpy as np
import pandas as pd

from bench_tool import JobReport, TemplateBenchData, BenchDatapoint, GH_DATE_FORMAT, ENSO_COMMIT_BASE_URL, JinjaData

_logger = logging.getLogger(__name__)

def create_template_data(
        job_reports_per_branch: Dict[str, List[JobReport]],
        bench_labels: Set[str]) -> List[TemplateBenchData]:
    """
    Creates all the necessary data for the Jinja template from all collected
    benchmark job reports.
    :param job_reports_per_branch: Mapping of branch name to list of job reports.
    job reports should be sorted by the commit date, otherwise the difference
    between scores might be wrongly computed.
    :param bench_labels:
    :return:
    """

    def pct_to_str(score_diff_perc: float) -> str:
        if not np.isnan(score_diff_perc):
            buff = "+" if score_diff_perc > 0 else ""
            buff += "{:.5f}".format(score_diff_perc * 100)
            buff += "%"
            return buff
        else:
            return "NaN"

    def diff_str(score_diff: float, score_diff_perc: float) -> str:
        if not np.isnan(score_diff):
            diff_str = "+" if score_diff > 0 else ""
            diff_str += "{:.5f}".format(score_diff)
            diff_str += " ("
            diff_str += pct_to_str(score_diff_perc)
            diff_str += ")"
            return diff_str
        else:
            return "NA"

    template_bench_datas: List[TemplateBenchData] = []
    for bench_label in bench_labels:
        _logger.debug("Creating template data for benchmark %s", bench_label)
        branch_datapoints: Dict[str, List[BenchDatapoint]] = {}
        for branch, job_reports in job_reports_per_branch.items():
            _logger.debug("Creating datapoints for branch %s from %d job reports",
                          branch, len(job_reports))
            datapoints: List[BenchDatapoint] = []
            for job_report in job_reports:
                prev_datapoint: Optional[BenchDatapoint] = \
                    datapoints[-1] if len(datapoints) > 0 else None
                if bench_label in job_report.label_score_dict:
                    score = job_report.label_score_dict[bench_label]
                    commit = job_report.bench_run.head_commit
                    timestamp = datetime.strptime(
                        commit.timestamp,
                        GH_DATE_FORMAT
                    )
                    commit_msg_header = \
                        commit.message.splitlines()[0].replace('"', "'")
                    series = pd.Series([
                        prev_datapoint.score if prev_datapoint else None,
                        score
                    ])
                    score_diff = series.diff()[1]
                    score_diff_perc = series.pct_change()[1]
                    tooltip = "score = " + str(score) + "\\n"
                    tooltip += "date = " + str(timestamp) + "\\n"
                    tooltip += "branch = " + branch + "\\n"
                    tooltip += "diff = " + diff_str(score_diff, score_diff_perc)
                    author_name = commit.author.name\
                        .replace('"', '\\"')\
                        .replace("'", "\\'")
                    datapoints.append(BenchDatapoint(
                        timestamp=timestamp,
                        score=score,
                        score_diff=str(score_diff),
                        score_diff_perc=pct_to_str(score_diff_perc),
                        tooltip=tooltip,
                        bench_run_url=job_report.bench_run.html_url,
                        commit_id=commit.id,
                        commit_msg=commit_msg_header,
                        commit_author=author_name,
                        commit_url=ENSO_COMMIT_BASE_URL + commit.id,
                    ))
            _logger.debug("%d datapoints created for branch %s",
                          len(datapoints), branch)
            branch_datapoints[branch] = datapoints.copy()
        _logger.debug("Template data for benchmark %s created", bench_label)
        template_bench_datas.append(TemplateBenchData(
            id=_label_to_id(bench_label),
            name=_label_to_name(bench_label),
            branches_datapoints=branch_datapoints,
        ))
    return template_bench_datas


def render_html(jinja_data: JinjaData, template_file: str, html_out_fname: str) -> None:
    jinja_env = jinja2.Environment(loader=jinja2.FileSystemLoader("."))
    jinja_template = jinja_env.get_template(template_file)
    generated_html = jinja_template.render(jinja_data.__dict__)
    if path.exists(html_out_fname):
        _logger.info("%s already exist, rewritting", html_out_fname)
    with open(html_out_fname, "w") as html_file:
        html_file.write(generated_html)


def _label_to_id(label: str) -> str:
    return label.replace(".", "_")


def _label_to_name(label: str) -> str:
    items = label.split(".")
    assert len(items) >= 2
    filtered_items = \
        [item for item in items if item not in (
            "org",
            "enso",
            "benchmark",
            "benchmarks",
            "semantic",
            "interpreter",
            "bench"
        )]
    return "_".join(filtered_items)
