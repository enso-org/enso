#!/usr/bin/env python

"""
Script for downloading Engine benchmark results into a single static web page
that visualizes all the benchmarks. Without any options, downloads and
visualizes benchmark data for the last 14 days. By default, no data is written
to the disk except for the generated web page, and the data are downloaded
asynchronously.

Set the `--source` parameter to either `engine` or `stdlib`.

The generated website is placed under "generated_site" directory

The default GH artifact retention period is 3 months, which means that all
the artifacts older than 3 months are dropped. If you wish to gather the data
for benchmarks older than 3 months, make sure that the `use_cache` parameter
is set to true, and that the cache directory is populated with older data.
If the script encounters an expired artifact, it prints a warning.

This script is under continuous development, so it is advised to use
`-v|--verbose` option all the time.

It queries only successful benchmark runs. If there are no successful benchmarks
in a given period, no results will be written.

The process of the script is roughly as follows:
- Asynchronously gather all the benchmark results from GH API into job reports (JobReport dataclass)
    - Use cache if possible to avoid unnecessary GH API queries
- Transform the gathered results into data for a particular benchmark sorted
  by an appropriate commit timestamp.
    - BenchmarkData class

If you wish to inspect the data yourself, just use --create-csv option.

Dependencies for the script:
- GH CLI utility
    - https://cli.github.com/
    - Used for convenience to do the GH API queries.
    - It needs to be installed, and you should also authenticate.
- Python version >= 3.7
- Python 3rd party packages:
    - pandas
        - Used for convenience for a very simple data processing
    - jinja2
        - Used as a template engine for the HTML.
"""

import asyncio
import json
import logging
import logging.config
import math
import os
import re
import shutil
import subprocess
import sys
import tempfile
import zipfile
from argparse import ArgumentParser, RawDescriptionHelpFormatter
from csv import DictWriter
from datetime import datetime, timedelta
from enum import Enum
from os import path
from typing import List, Dict, Optional, Any, Union, Set
from dataclasses import dataclass
import xml.etree.ElementTree as ET


if not (sys.version_info.major >= 3 and sys.version_info.minor >= 7):
    print("ERROR: python version lower than 3.7")
    exit(1)
try:
    import pandas as pd
    import numpy as np
    import jinja2
except ModuleNotFoundError as err:
    print("ERROR: One of pandas, numpy, or jinja2 packages not installed", file=sys.stderr)
    exit(1)

DATE_FORMAT = "%Y-%m-%d"
ENGINE_BENCH_WORKFLOW_ID = 29450898
"""
Workflow ID of engine benchmarks, got via `gh api 
'/repos/enso-org/enso/actions/workflows'`.
The name of the workflow is 'Benchmark Engine'
"""
NEW_ENGINE_BENCH_WORKFLOW_ID = 67075764
"""
Workflow ID for 'Benchmark Engine' workflow, which is the new workflow
since 2023-08-22.
"""
STDLIBS_BENCH_WORKFLOW_ID = 66661001
"""
Workflow ID of stdlibs benchmarks, got via `gh api 
'/repos/enso-org/enso/actions/workflows'`.
The name is 'Benchmark Standard Libraries'
"""
GH_DATE_FORMAT = "%Y-%m-%dT%H:%M:%SZ"
""" Date format as returned from responses in GH API"""
ENSO_COMMIT_BASE_URL = "https://github.com/enso-org/enso/commit/"
JINJA_TEMPLATE = "templates/template_jinja.html"
""" Path to the Jinja HTML template """
TEMPLATES_DIR = "templates"
GENERATED_SITE_DIR = "generated_site"
GH_ARTIFACT_RETENTION_PERIOD = timedelta(days=90)


class Source(Enum):
    ENGINE = "engine"
    STDLIB = "stdlib"

    def workflow_ids(self) -> List[int]:
        if self == Source.ENGINE:
            return [ENGINE_BENCH_WORKFLOW_ID, NEW_ENGINE_BENCH_WORKFLOW_ID]
        elif self == Source.STDLIB:
            return [STDLIBS_BENCH_WORKFLOW_ID]
        else:
            raise ValueError(f"Unknown source {self}")


@dataclass
class Author:
    name: str


@dataclass
class Commit:
    """ Corresponds to the commit from GH API """
    id: str
    author: Author
    timestamp: str
    message: str


@dataclass
class JobRun:
    """
    Gathered via the GH API. Defines a single run of an Engine benchmark job.
    """
    id: str
    display_title: str
    html_url: str
    run_attempt: int
    """ An event as defined by the GitHub API, for example 'push' or 'schedule' """
    event: str
    head_commit: Commit


@dataclass
class JobReport:
    """
    Gathered via the GH API - a report that is pushed as an aritfact to the job.
    Contains a XML file with scores for all the benchmarks.
    """
    label_score_dict: Dict[str, float]
    """ A mapping of benchmark labels to their scores """
    bench_run: JobRun


@dataclass
class BenchmarkData:
    """
    Data for a single benchmark compiled from all the job reports.
    """

    @dataclass
    class Entry:
        score: float
        commit: Commit
        bench_run_url: str
        bench_run_event: str

    label: str
    """ Label for the benchmark, as reported by org.enso.interpreter.bench.BenchmarksRunner """
    entries: List[Entry]
    """ Entries sorted by timestamps """


@dataclass
class BenchDatapoint:
    """
    A single datapoint that will be on the chart. `timestamp` is on X axis,
    `score` on Y axis, and the rest of the fields is used either for the tooltip,
    or for the selection info.
    """
    timestamp: datetime
    score: float
    score_diff: str
    """ Difference of the score with previous datapoint, or NaN """
    score_diff_perc: str
    tooltip: str
    bench_run_url: str
    commit_id: str
    commit_msg: str
    commit_author: str
    commit_url: str


@dataclass
class TemplateBenchData:
    """ Data for one benchmark label (with a unique name and ID) """
    id: str
    """ ID of the benchmark, must not contain dots """
    name: str
    """ Human readable name of the benchmark """
    branches_datapoints: Dict[str, List[BenchDatapoint]]
    """ Mapping of branches to datapoints for that branch """


@dataclass
class JinjaData:
    bench_source: Source
    bench_datas: List[TemplateBenchData]
    branches: List[str]
    since: datetime
    until: datetime
    display_since: datetime
    """ The date from which all the datapoints are first displayed """


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


def _parse_bench_report_from_json(obj: Dict[Any, Any]) -> JobReport:
    return JobReport(
        bench_run=_parse_bench_run_from_json(obj["bench_run"]),
        label_score_dict=obj["label_score_dict"]
    )


def _bench_report_to_json(bench_report: JobReport) -> Dict[Any, Any]:
    return {
        "bench_run": {
            "id": bench_report.bench_run.id,
            "html_url": bench_report.bench_run.html_url,
            "run_attempt": bench_report.bench_run.run_attempt,
            "event": bench_report.bench_run.event,
            "display_title": bench_report.bench_run.display_title,
            "head_commit": {
                "id": bench_report.bench_run.head_commit.id,
                "message": bench_report.bench_run.head_commit.message,
                "timestamp": bench_report.bench_run.head_commit.timestamp,
                "author": {
                    "name": bench_report.bench_run.head_commit.author.name
                }
            }
        },
        "label_score_dict": bench_report.label_score_dict
    }


def _parse_bench_report_from_xml(bench_report_xml_path: str, bench_run: JobRun) -> "JobReport":
    logging.debug(f"Parsing BenchReport from {bench_report_xml_path}")
    tree = ET.parse(bench_report_xml_path)
    root = tree.getroot()
    label_score_dict: Dict[str, float] = dict()
    for cases in root:
        assert cases.tag == "cases"
        for case in cases:
            assert case.tag == "case"
            label = case.findtext("label").strip()
            scores = case.find("scores")
            scores_float = [float(score.text.strip()) for score in scores]
            if len(scores_float) > 1:
                logging.warning(f"More than one score for benchmark {label}, "
                                f"using the best one (the smallest one).")
            label_score_dict[label] = min(scores_float)
    return JobReport(
        label_score_dict=label_score_dict,
        bench_run=bench_run
    )


def _is_benchrun_id(name: str) -> bool:
    return re.match("\d{9}", name) is not None


def _read_json(json_file: str) -> Dict[Any, Any]:
    assert path.exists(json_file) and path.isfile(json_file)
    with open(json_file, "r") as f:
        return json.load(f)


async def _invoke_gh_api(endpoint: str,
                   query_params: Dict[str, str] = {},
                   result_as_text: bool = True) -> Union[Dict[str, Any], bytes]:
    query_str_list = [key + "=" + value for key, value in query_params.items()]
    query_str = "&".join(query_str_list)
    cmd = [
        "gh",
        "api",
        f"/repos/enso-org/enso{endpoint}" + ("" if len(query_str) == 0 else "?" + query_str)
    ]
    logging.info(f"Starting subprocess `{' '.join(cmd)}`")
    proc = await asyncio.create_subprocess_exec("gh", *cmd[1:],
                                                stdout=subprocess.PIPE,
                                                stderr=subprocess.PIPE)
    out, err = await proc.communicate()
    logging.info(f"Finished subprocess `{' '.join(cmd)}`")
    if proc.returncode != 0:
        print("Command `" + " ".join(cmd) + "` FAILED with errcode " + str(
            proc.returncode))
        print(err.decode())
        exit(proc.returncode)
    if result_as_text:
        return json.loads(out.decode())
    else:
        return out


class Cache:
    """
    Cache is a directory filled with json files that have name of format <bench_run_id>.json, and
    in every json, there is `BenchReport` dataclass serialized.
    """

    def __init__(self, dirname: str):
        assert path.exists(dirname) and path.isdir(dirname)
        self._dir = dirname
        # Keys are BenchRun ids
        self._items: Dict[str, JobReport] = {}
        for fname in os.listdir(dirname):
            fname_without_ext, ext = path.splitext(fname)
            if _is_benchrun_id(fname_without_ext) and ext == ".json":
                logging.debug(f"Loading into cache from {fname}")
                bench_report = _parse_bench_report_from_json(
                    _read_json(path.join(dirname, fname))
                )
                self._items[fname_without_ext] = bench_report

    def __len__(self) -> int:
        return len(self._items)

    def __contains__(self, key: str) -> bool:
        assert _is_benchrun_id(key)
        return key in self._items

    def __getitem__(self, item: str) -> Optional[JobReport]:
        if not _is_benchrun_id(item):
            return None
        else:
            return self._items[item]

    def __setitem__(self, bench_run_id: str, bench_report: JobReport) -> None:
        assert isinstance(bench_report, JobReport)
        assert isinstance(bench_run_id, str)
        assert _is_benchrun_id(bench_run_id)
        self._items[bench_run_id] = bench_report
        json_fname = path.join(self._dir, bench_run_id + ".json")
        logging.debug(f"Putting {bench_run_id} into cache {json_fname}")
        with open(json_fname, "w") as json_file:
            json.dump(
                _bench_report_to_json(bench_report),
                json_file,
                indent=2,
                ensure_ascii=False
            )

    def __str__(self) -> str:
        return str(self._items)

    def contains(self, bench_run_id: str) -> bool:
        return bench_run_id in self._items


class FakeCache:
    def __getitem__(self, item):
        return None

    def __setitem__(self, key, value):
        pass

    def __contains__(self, item):
        return False

    def __len__(self):
        return 0


async def get_bench_runs(since: datetime, until: datetime, branch: str, workflow_id: int) -> List[JobRun]:
    """
    Fetches the list of all the job runs from the GH API for the specified `branch`.
    """
    logging.info(f"Looking for all successful Engine benchmark workflow run "
                 f"actions from {since} to {until} for branch {branch} "
                 f"and workflow ID {workflow_id}")
    query_fields = {
        "branch": branch,
        "status": "success",
        "created": since.strftime(DATE_FORMAT) + ".." + until.strftime(DATE_FORMAT),
        # Start with 1, just to determine the total count
        "per_page": "1"
    }
    res = await _invoke_gh_api(f"/actions/workflows/{workflow_id}/runs", query_fields)
    total_count = int(res["total_count"])
    per_page = 3
    logging.debug(f"Total count of all runs: {total_count} for workflow ID "
                  f"{workflow_id}. Will process {per_page} runs per page")

    async def get_and_parse_run(page: int, parsed_bench_runs) -> None:
        _query_fields = query_fields.copy()
        _query_fields["page"] = str(page)
        res = await _invoke_gh_api(f"/actions/workflows/{workflow_id}/runs", _query_fields)
        bench_runs_json = res["workflow_runs"]
        _parsed_bench_runs = [_parse_bench_run_from_json(bench_run_json)
                              for bench_run_json in bench_runs_json]
        parsed_bench_runs.extend(_parsed_bench_runs)

    # Now we know the total count, so we can fetch all the runs
    query_fields["per_page"] = str(per_page)
    num_queries = math.ceil(total_count / per_page)
    parsed_bench_runs = []

    tasks = []
    # Page is indexed from 1
    for page in range(1, num_queries + 1):
        tasks.append(get_and_parse_run(page, parsed_bench_runs))
    await asyncio.gather(*tasks)

    return parsed_bench_runs


async def get_bench_report(bench_run: JobRun, cache: Cache, temp_dir: str) -> Optional[JobReport]:
    """
    Extracts some data from the given bench_run, which was fetched via the GH API,
    optionally getting it from the cache.
    An artifact in GH can expire, in such case, returns None.
    :param bench_run:
    :param cache:
    :param temp_dir: Used for downloading and unzipping artifacts.
    :return: None if the corresponding artifact expired.
    """
    if bench_run.id in cache:
        logging.info(f"Getting bench run with ID {bench_run.id} from cache")
        return cache[bench_run.id]

    # There might be multiple artifacts in the artifact list for a benchmark run
    # We are looking for the one named 'Runtime Benchmark Report', which will
    # be downloaded as a ZIP file.
    obj: Dict[str, Any] = await _invoke_gh_api(f"/actions/runs/{bench_run.id}/artifacts")
    artifacts = obj["artifacts"]
    assert len(artifacts) == 1, "There should be exactly one artifact for a benchmark run"
    bench_report_artifact = artifacts[0]
    assert bench_report_artifact, "Benchmark Report artifact not found"
    artifact_id = str(bench_report_artifact["id"])
    if bench_report_artifact["expired"]:
        created_at = bench_report_artifact["created_at"]
        updated_at = bench_report_artifact["updated_at"]
        expires_at = bench_report_artifact["expires_at"]
        logging.warning(f"Artifact with ID {artifact_id} from bench report {bench_run.id} has expired. "
                        f"created_at={created_at}, updated_at={updated_at}, expires_at={expires_at}")
        return None

    # Get contents of the ZIP artifact file
    artifact_ret = await _invoke_gh_api(f"/actions/artifacts/{artifact_id}/zip", result_as_text=False)
    zip_file_name = os.path.join(temp_dir, artifact_id + ".zip")
    logging.debug(f"Writing artifact ZIP content into {zip_file_name}")
    with open(zip_file_name, "wb") as zip_file:
        zip_file.write(artifact_ret)

    extracted_dirname = os.path.join(temp_dir, artifact_id)
    if os.path.exists(extracted_dirname):
        shutil.rmtree(extracted_dirname)
    os.mkdir(extracted_dirname)

    logging.debug(f"Extracting {zip_file_name} into {extracted_dirname}")
    zip_file = zipfile.ZipFile(zip_file_name, "r")
    zip_file.extractall(extracted_dirname)
    bench_report_xml = path.join(extracted_dirname, "bench-report.xml")
    assert path.exists(bench_report_xml)

    bench_report_parsed = _parse_bench_report_from_xml(bench_report_xml, bench_run)
    cache[bench_run.id] = bench_report_parsed
    return bench_report_parsed


CSV_FIELDNAMES = [
    "label",
    "score",
    "commit_id",
    "commit_author",
    "commit_timestamp",
    "bench_run_url",
    "bench_run_event"
]


def write_bench_reports_to_csv(bench_reports: List[JobReport], csv_fname: str) -> None:
    logging.info(
        f"Writing {len(bench_reports)} benchmark reports to {csv_fname}")
    assert len(bench_reports) > 0
    if not path.exists(path.dirname(csv_fname)):
        logging.debug(f"Creating directory {path.dirname(csv_fname)}")
        os.mkdir(path.dirname(csv_fname))
    with open(csv_fname, "w") as csv_file:
        csv_writer = DictWriter(csv_file, CSV_FIELDNAMES)
        csv_writer.writeheader()
        for bench_report in bench_reports:
            for label, score in bench_report.label_score_dict.items():
                csv_writer.writerow({
                    "label": label,
                    "score": score,
                    "commit_id": bench_report.bench_run.head_commit.id,
                    "commit_author": bench_report.bench_run.head_commit.author.name,
                    "commit_timestamp": bench_report.bench_run.head_commit.timestamp,
                    "bench_run_url": bench_report.bench_run.html_url,
                    "bench_run_event": bench_report.bench_run.event
                })


def populate_cache(cache_dir: str) -> Cache:
    """
    Initializes cache from `cache_dir`, if there are any items.
    See docs of `Cache`.

    :param cache_dir: Path to the cache directory. Does not have to exist
    :return: Populated cache. Might be empty.
    """
    if not path.exists(cache_dir):
        logging.info(f"No cache at {cache_dir}, creating the cache directory")
        os.mkdir(cache_dir)
    logging.debug(f"Initializing cache from {cache_dir}")
    cache = Cache(cache_dir)
    logging.debug(f"Cache populated with {len(cache)} items")
    return cache


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
        logging.debug(f"Creating template data for benchmark {bench_label}")
        branch_datapoints: Dict[str, List[BenchDatapoint]] = {}
        for branch, job_reports in job_reports_per_branch.items():
            logging.debug(f"Creating datapoints for branch {branch} from {len(job_reports)} job reports")
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
            logging.debug(f"{len(datapoints)} datapoints created for branch {branch}")
            branch_datapoints[branch] = datapoints.copy()
        logging.debug(f"Template data for benchmark {bench_label} created")
        template_bench_datas.append(TemplateBenchData(
            id=_label_to_id(bench_label),
            name=_label_to_name(bench_label),
            branches_datapoints=branch_datapoints,
        ))
    return template_bench_datas


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


def _gather_all_bench_labels(job_reports: List[JobReport]) -> Set[str]:
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


def render_html(jinja_data: JinjaData, template_file: str, html_out_fname: str) -> None:
    jinja_env = jinja2.Environment(loader=jinja2.FileSystemLoader("."))
    jinja_template = jinja_env.get_template(template_file)
    generated_html = jinja_template.render(jinja_data.__dict__)
    if path.exists(html_out_fname):
        logging.info(f"{html_out_fname} already exist, rewritting")
    with open(html_out_fname, "w") as html_file:
        html_file.write(generated_html)


def ensure_gh_installed() -> None:
    try:
        out = subprocess.run(["gh", "--version"], check=True, capture_output=True)
        if out.returncode != 0:
            print("`gh` command not found - GH CLI utility is not installed. "
                  "See https://cli.github.com/", file=sys.stderr)
            exit(1)
    except subprocess.CalledProcessError:
        print("`gh` command not found - GH CLI utility is not installed. "
              "See https://cli.github.com/", file=sys.stderr)
        exit(1)


async def main():
    default_since: datetime = (datetime.now() - timedelta(days=14))
    default_until: datetime = datetime.now()
    default_cache_dir = path.expanduser("~/.cache/enso_bench_download")
    default_csv_out = "Engine_Benchs/data/benchs.csv"
    date_format_help = DATE_FORMAT.replace("%", "%%")

    def _parse_bench_source(_bench_source: str) -> Source:
        try:
            return Source(_bench_source)
        except ValueError:
            print(f"Invalid benchmark source {_bench_source}.", file=sys.stderr)
            print(f"Available sources: {[source.value for source in Source]}", file=sys.stderr)
            exit(1)

    arg_parser = ArgumentParser(description=__doc__,
                                formatter_class=RawDescriptionHelpFormatter)
    arg_parser.add_argument("-v", "--verbose", action="store_true")
    arg_parser.add_argument("-s", "--source",
                            action="store",
                            required=True,
                            metavar=f"({Source.ENGINE.value}|{Source.STDLIB.value})",
                            type=lambda s: _parse_bench_source(s),
                            help=f"The source of the benchmarks. Available sources: "
                                 f"{[source.value for source in Source]}")
    arg_parser.add_argument("--since", action="store",
                            default=default_since,
                            metavar="SINCE_DATE",
                            type=lambda s: datetime.strptime(s, DATE_FORMAT),
                            help=f"The date from which the benchmark results will be gathered. "
                                 f"Format is {date_format_help}. "
                                 f"The default is 14 days before")
    arg_parser.add_argument("--until", action="store",
                            default=default_until,
                            metavar="UNTIL_DATE",
                            type=lambda s: datetime.strptime(s, DATE_FORMAT),
                            help=f"The date until which the benchmark results will be gathered. "
                                 f"Format is {date_format_help}. "
                                 f"The default is today")
    arg_parser.add_argument("--use-cache",
                            default=False,
                            metavar="(true|false)",
                            type=lambda input: True if input in ("true", "True") else False,
                            help="Whether the cache directory should be used. The default is False.")
    arg_parser.add_argument("-c", "--cache", action="store",
                            default=default_cache_dir,
                            metavar="CACHE_DIR",
                            help=f"Cache directory. Makes sense only iff specified with --use-cache argument. "
                                 f"The default is {default_cache_dir}. If there are any troubles with the "
                                 f"cache, just do `rm -rf {default_cache_dir}`.")
    arg_parser.add_argument("-b", "--branches", action="store",
                            nargs="+",
                            default=["develop"],
                            help="List of branches to gather the benchmark results from. "
                                 "The default is ['develop']")
    arg_parser.add_argument("-l", "--labels", action="store",
                            nargs="+",
                            default=set(),
                            help="List of labels to gather the benchmark results from."
                                 "The default behavior is to gather all the labels")
    arg_parser.add_argument("-t", "--tmp-dir", action="store",
                            default=None,
                            help="Temporary directory with default created by `tempfile.mkdtemp()`")
    arg_parser.add_argument("--create-csv", action="store_true",
                            default=False,
                            help="Whether an intermediate `benchs.csv` should be created. "
                                 "Appropriate to see whether the benchmark downloading was successful. "
                                 "Or if you wish to inspect the CSV with Enso")
    arg_parser.add_argument("--csv-output",
                            default=default_csv_out,
                            metavar="CSV_OUTPUT",
                            help="Output CSV file. Makes sense only when used with --create-csv argument")
    args = arg_parser.parse_args()
    if args.verbose:
        log_level = logging.DEBUG
    else:
        log_level = logging.INFO
    logging.basicConfig(level=log_level, stream=sys.stdout)

    since: datetime = args.since
    until: datetime = args.until
    cache_dir: str = args.cache
    if not args.tmp_dir:
        temp_dir: str = tempfile.mkdtemp()
    else:
        temp_dir: str = args.tmp_dir
    use_cache: bool = args.use_cache
    assert cache_dir and temp_dir
    bench_source: Source = args.source
    csv_output: str = args.csv_output
    create_csv: bool = args.create_csv
    branches: List[str] = args.branches
    labels_override: Set[str] = args.labels
    logging.debug(f"parsed args: since={since}, until={until}, cache_dir={cache_dir}, "
                 f"temp_dir={temp_dir}, use_cache={use_cache}, bench_source={bench_source}, "
                 f"csv_output={csv_output}, "
                 f"create_csv={create_csv}, branches={branches}, "
                 f"labels_override={labels_override}")

    ensure_gh_installed()

    # If the user requires benchmarks for which artifacts are not retained
    # anymore, then cache should be used.
    min_since_without_cache = datetime.today() - GH_ARTIFACT_RETENTION_PERIOD
    if not use_cache and since < min_since_without_cache:
        logging.warning(f"The default GH artifact retention period is "
                        f"{GH_ARTIFACT_RETENTION_PERIOD.days} days. "
                        f"This means that all the artifacts older than "
                        f"{min_since_without_cache.date()} are expired."
                        f"The use_cache parameter is set to False, so no "
                        f"expired artifacts will be fetched.")
        logging.warning(f"The `since` parameter is reset to "
                        f"{min_since_without_cache.date()} to prevent "
                        f"unnecessary GH API queries.")
        since = min_since_without_cache

    if use_cache:
        cache = populate_cache(cache_dir)
    else:
        cache = FakeCache()

    bench_labels: Optional[Set[str]] = None
    """ Set of all gathered benchmark labels from all the job reports """
    job_reports_per_branch: Dict[str, List[JobReport]] = {}
    for branch in branches:
        bench_runs: List[JobRun] = []
        for workflow_id in bench_source.workflow_ids():
            bench_runs.extend(
                await get_bench_runs(since, until, branch, workflow_id)
            )
        if len(bench_runs) == 0:
            print(
                f"No successful benchmarks found within period since {since}"
                f" until {until} for branch {branch}")
            exit(1)

        job_reports: List[JobReport] = []

        async def _process_report(_bench_run):
            _job_report = await get_bench_report(_bench_run, cache, temp_dir)
            if _job_report:
                job_reports.append(_job_report)

        tasks = []
        for bench_run in bench_runs:
            tasks.append(_process_report(bench_run))
        await asyncio.gather(*tasks)

        logging.debug(f"Got {len(job_reports)} job reports for branch {branch}")
        if len(job_reports) == 0:
            print(f"There were 0 job_reports in the specified time interval, "
                  f"for branch {branch}, so "
                  "there is nothing to visualize or compare.")
            exit(1)

        logging.debug("Sorting job_reports by commit date")

        def _get_timestamp(job_report: JobReport) -> datetime:
            return datetime.strptime(
                job_report.bench_run.head_commit.timestamp,
                GH_DATE_FORMAT
            )

        job_reports.sort(key=lambda report: _get_timestamp(report))

        if create_csv:
            write_bench_reports_to_csv(job_reports, csv_output)
            logging.info(f"Benchmarks written to {csv_output}")
            print(f"The generated CSV is in {csv_output}")
            exit(0)

        # Gather all the benchmark labels from all the job reports
        if bench_labels is None:
            all_bench_labels = _gather_all_bench_labels(job_reports)
            if len(labels_override) > 0:
                logging.info(f"Subset of labels specified: {labels_override}")
                if not set(labels_override).issubset(all_bench_labels):
                    print(f"Specified bench labels {labels_override} are not a subset of "
                          f"all bench labels {all_bench_labels}")
                    exit(1)
                bench_labels = labels_override
            else:
                bench_labels = all_bench_labels
        logging.debug(f"Gathered bench_labels: {bench_labels}")

        job_reports_per_branch[branch] = job_reports

    template_bench_datas: List[TemplateBenchData] = \
        create_template_data(job_reports_per_branch, bench_labels)
    template_bench_datas.sort(key=lambda data: data.id)

    jinja_data = JinjaData(
        since=since,
        display_since=max(until - timedelta(days=30), since),
        until=until,
        bench_datas=template_bench_datas,
        bench_source=bench_source,
        branches=branches,
    )

    # Render Jinja template with jinja_data
    if not path.exists(GENERATED_SITE_DIR):
        os.mkdir(GENERATED_SITE_DIR)

    logging.debug(f"Rendering HTML from {JINJA_TEMPLATE} to {GENERATED_SITE_DIR}")
    site_path = path.join(GENERATED_SITE_DIR, bench_source.value + "-benchs.html")
    render_html(
        jinja_data,
        JINJA_TEMPLATE,
        site_path
    )
    logging.debug(f"Copying static site content from {TEMPLATES_DIR} to {GENERATED_SITE_DIR}")
    shutil.copy(
        path.join(TEMPLATES_DIR, "styles.css"),
        path.join(GENERATED_SITE_DIR, "styles.css")
    )

    index_html_abs_path = path.abspath(site_path)
    print(f"The generated HTML is in {index_html_abs_path}")
    print(f"Open file://{index_html_abs_path} in the browser")


if __name__ == "__main__":
    asyncio.run(main())
