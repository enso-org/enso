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

import sys

from bench_tool.remote_cache import RemoteCache, ReadonlyRemoteCache

if not (sys.version_info.major >= 3 and sys.version_info.minor >= 7):
    print("ERROR: python version lower than 3.7")
    exit(1)

import asyncio
import json
import logging
import logging.config
import math
import os
import re
import shutil
import tempfile
import zipfile
from argparse import ArgumentParser, RawDescriptionHelpFormatter
from csv import DictWriter
from datetime import datetime, timedelta
from os import path
from typing import List, Dict, Optional, Any, Set
import xml.etree.ElementTree as ET

from bench_tool import DATE_FORMAT, GENERATED_SITE_DIR, GH_DATE_FORMAT, GH_ARTIFACT_RETENTION_PERIOD, TEMPLATES_DIR, \
    JINJA_TEMPLATE, ENSO_REPO, Author, Commit, JobRun, JobReport, \
    TemplateBenchData, JinjaData, Source
from bench_tool.gh import invoke_gh_api, ensure_gh_installed
from bench_tool.template_render import create_template_data, render_html

try:
    import pandas as pd
    import numpy as np
    import jinja2
except ModuleNotFoundError as err:
    print("ERROR: One of pandas, numpy, or jinja2 packages not installed", file=sys.stderr)
    print("Install either with `pip install pandas numpy jinja2` or "
          "with `apt-get install python3-pandas python3-numpy python3-jinja2`", file=sys.stderr)
    exit(1)


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
                                f"using the last one (the newest one).")
            label_score_dict[label] = scores_float[len(scores_float) - 1]
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
    res = await invoke_gh_api(ENSO_REPO, f"/actions/workflows/{workflow_id}/runs", query_fields)
    total_count = int(res["total_count"])
    per_page = 3
    logging.debug(f"Total count of all runs: {total_count} for workflow ID "
                  f"{workflow_id}. Will process {per_page} runs per page")

    async def get_and_parse_run(page: int, parsed_bench_runs) -> None:
        _query_fields = query_fields.copy()
        _query_fields["page"] = str(page)
        res = await invoke_gh_api(ENSO_REPO, f"/actions/workflows/{workflow_id}/runs", _query_fields)
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


async def get_bench_report(bench_run: JobRun, temp_dir: str, remote_cache: RemoteCache) -> Optional[JobReport]:
    """
    Extracts some data from the given bench_run, which was fetched via the GH API,
    optionally getting it from the cache.
    An artifact in GH can expire, in such case, returns None.
    :param bench_run:
    :param temp_dir: Used for downloading and unzipping artifacts.
    :return: None if the corresponding artifact expired.
    """
    # There might be multiple artifacts in the artifact list for a benchmark run
    # We are looking for the one named 'Runtime Benchmark Report', which will
    # be downloaded as a ZIP file.
    obj: Dict[str, Any] = await invoke_gh_api(ENSO_REPO, f"/actions/runs/{bench_run.id}/artifacts")
    artifacts = obj["artifacts"]
    if len(artifacts) != 1:
        logging.warning("Bench run %s does not contain exactly one artifact, but it is a successful run.",
                      bench_run.id)
        return None
    bench_report_artifact = artifacts[0]
    assert bench_report_artifact, "Benchmark Report artifact not found"
    artifact_id = str(bench_report_artifact["id"])
    created_at = bench_report_artifact["created_at"]
    updated_at = bench_report_artifact["updated_at"]
    expires_at = bench_report_artifact["expires_at"]
    is_expired = bench_report_artifact["expired"]
    logging.debug(f"Got artifact with ID {artifact_id}, from bench run {bench_run.id}: "
                  f"created_at={created_at}, updated_at={updated_at}, expires_at={expires_at}, "
                  f"is_expired={is_expired}")

    job_report = await remote_cache.fetch(bench_run.id)
    if is_expired and job_report is None:
        logging.error(f"Artifact {artifact_id} from bench run {bench_run.id} is expired, and it is not in the remote cache")
        return None
    if job_report:
        logging.debug(f"Got job report from the cache for {bench_run.id}")
        return job_report

    assert not is_expired

    # Get contents of the ZIP artifact file
    artifact_ret = await invoke_gh_api(ENSO_REPO, f"/actions/artifacts/{artifact_id}/zip", result_as_json=False)
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
    await remote_cache.put(bench_run.id, bench_report_parsed)
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


async def main():
    default_since: datetime = (datetime.now() - timedelta(days=14))
    default_until: datetime = datetime.now()
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
    if not args.tmp_dir:
        temp_dir: str = tempfile.mkdtemp()
    else:
        temp_dir: str = args.tmp_dir
    bench_source: Source = args.source
    csv_output: str = args.csv_output
    create_csv: bool = args.create_csv
    branches: List[str] = args.branches
    labels_override: Set[str] = args.labels
    logging.debug(f"parsed args: since={since}, until={until}, "
                 f"temp_dir={temp_dir}, bench_source={bench_source}, "
                 f"csv_output={csv_output}, "
                 f"create_csv={create_csv}, branches={branches}, "
                 f"labels_override={labels_override}")

    ensure_gh_installed()

    # If the user requires benchmarks for which artifacts are not retained
    # anymore, then cache should be used.
    min_since_without_cache = datetime.today() - GH_ARTIFACT_RETENTION_PERIOD
    if since < min_since_without_cache:
        logging.info(f"The default GH artifact retention period is "
                        f"{GH_ARTIFACT_RETENTION_PERIOD.days} days. "
                        f"This means that all the artifacts older than "
                        f"{min_since_without_cache.date()} are expired."
                        f"The since date was set to {since}, so the remote cache is enabled, "
                        f"and the older artifacts will be fetched from the cache.")

    remote_cache = ReadonlyRemoteCache()
    await remote_cache.initialize()

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
            _job_report = await get_bench_report(_bench_run, temp_dir, remote_cache)
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
    site_path = GENERATED_SITE_DIR.joinpath(bench_source.value + "-benchs.html")
    render_html(
        jinja_data,
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
