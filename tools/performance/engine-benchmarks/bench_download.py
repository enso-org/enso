#!/usr/bin/env python

"""
Script for downloading Engine benchmark results into a single static web page
that visualizes all the benchmarks. Without any options, downloads and
visualizes benchmark data for the last 14 days.

It downloads the data synchronously and uses a cache directory by default.
It is advised to use `-v|--verbose` option all the time.

It queries only successful benchmark runs. If there are no successful benchmarks
in a given period, no results will be written.

The process of the script is roughly as follows:
- Gather all the benchmark results from GH API into job reports (JobReport dataclass)
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

import json
import logging
import logging.config
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
from os import path
from typing import List, Dict, Optional, Any, Union
from dataclasses import dataclass
if not (sys.version_info.major >= 3 and sys.version_info.minor >= 7):
    print("ERROR: python version lower than 3.7")
    exit(1)
try:
    import pandas as pd
    import numpy as np
    import jinja2
except ModuleNotFoundError as err:
    print("ERROR: One of pandas, numpy, or jinja2 packages not installed")
    exit(1)


BENCH_RUN_NAME = "Benchmark Engine"
DATE_FORMAT = "%Y-%m-%d"
# Workflod ID of engine benchmarks, got via `gh api '/repos/enso-org/enso/actions/workflows'`
BENCH_WORKFLOW_ID = 29450898
GH_DATE_FORMAT = "%Y-%m-%dT%H:%M:%SZ"
""" Date format as returned from responses in GH API"""
ENSO_COMMIT_BASE_URL = "https://github.com/enso-org/enso/commit/"
JINJA_TEMPLATE = "template_jinja.html"
""" Path to the Jinja HTML template """


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
        commit_id: str
        commit_msg: str
        commit_url: str
        commit_author: str
        commit_timestamp: datetime
        bench_run_url: str
        bench_run_event: str
    label: str
    """ Label for the benchmark, as reported by org.enso.interpreter.bench.BenchmarksRunner """
    entries: List[Entry]
    """ Entries sorted by timestamps """


@dataclass
class ChartRow:
    timestamp: datetime
    score: float
    tooltip: str


@dataclass
class TemplateBenchData:
    id: str
    rows: List[ChartRow]
    score_diffs: List[str]
    """ S string that is displayed in the selection info """
    commit_ids: List[str]
    commit_msgs: List[str]
    commit_authors: List[str]
    commit_urls: List[str]
    bench_run_urls: List[str]
    """ URLs to Engine benchmark job """


@dataclass
class JinjaData:
    bench_datas: List[TemplateBenchData]
    since: datetime
    until: datetime


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


def _parse_bench_report_from_xml(bench_report_xml: str, bench_run: JobRun) -> "JobReport":
    logging.debug(f"Parsing BenchReport from {bench_report_xml}")
    with open(bench_report_xml, "r") as f:
        lines = f.readlines()
    label_pattern = re.compile("<label>(?P<label>.+)</label>")
    score_pattern = re.compile("<score>(?P<score>.+)</score>")
    label_score_dict = {}
    for line in lines:
        line = line.strip()
        label_match = label_pattern.match(line)
        score_match = score_pattern.match(line)
        if label_match:
            label = label_match.group("label")
        if score_match:
            score = score_match.group("score")
            assert label, "label element must be before score element"
            label_score_dict[label] = float(score)
    return JobReport(
        label_score_dict=label_score_dict,
        bench_run=bench_run
    )


def _is_benchrun_id(name: str) -> bool:
    return re.match("[0-9]{9}", name) is not None


def _read_json(json_file: str) -> Dict[Any, Any]:
    assert path.exists(json_file) and path.isfile(json_file)
    with open(json_file, "r") as f:
        return json.load(f)


def _invoke_gh_api(endpoint: str,
                   query_params: Dict[str, str] = {},
                   result_as_text: bool = True) -> Union[Dict[Any, Any], bytes]:
    query_str_list = [key + "=" + value for key, value in query_params.items()]
    query_str = "&".join(query_str_list)
    cmd = [
        "gh",
        "api",
        f"/repos/enso-org/enso{endpoint}" + ("" if len(query_str) == 0 else "?" + query_str)
    ]
    logging.info(f"Running subprocess `{' '.join(cmd)}`")
    try:
        ret = subprocess.run(cmd, check=True, text=result_as_text, capture_output=True)
        if result_as_text:
            return json.loads(ret.stdout)
        else:
            return ret.stdout
    except subprocess.CalledProcessError as err:
        print("Command `" + " ".join(cmd) + "` FAILED with errcode " + str(err.returncode))
        print(err.stdout)
        print(err.stderr)
        exit(err.returncode)


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


def get_bench_runs(since: datetime, until: datetime) -> List[JobRun]:
    logging.info(f"Looking for all successful Engine benchmark workflow run actions from {since} to {until}")
    query_fields = {
        "branch": "develop",
        "status": "success",
        "created": since.strftime("%Y-%m-%d") + ".." + until.strftime("%Y-%m-%d"),
        # Start with 1, just to determine the total count
        "per_page": "1"
    }
    res = _invoke_gh_api(f"/actions/workflows/{BENCH_WORKFLOW_ID}/runs", query_fields)
    total_count = int(res["total_count"])
    per_page = 30
    logging.debug(f"Total count of all runs: {total_count}, will process {per_page} runs per page")

    query_fields["per_page"] = str(per_page)
    processed = 0
    page = 1
    parsed_bench_runs = []
    while processed < total_count:
        logging.debug(f"Processing page {page}, processed={processed}, total_count={total_count}")
        query_fields["page"] = str(page)
        res = _invoke_gh_api(f"/actions/workflows/{BENCH_WORKFLOW_ID}/runs", query_fields)
        bench_runs_json = res["workflow_runs"]
        parsed_bench_runs += [_parse_bench_run_from_json(bench_run_json) for bench_run_json in bench_runs_json]
        processed += per_page
        page += 1

    return parsed_bench_runs


def get_bench_report(bench_run: JobRun, cache: Cache, temp_dir: str) -> Optional[JobReport]:
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

    logging.info(f"Getting bench run with ID {bench_run.id} from GitHub API")
    # There might be multiple artifacts in the artifact list for a benchmark run
    # We are looking for the one named 'Runtime Benchmark Report', which will
    # be downloaded as a ZIP file.
    obj = _invoke_gh_api(f"/actions/runs/{bench_run.id}/artifacts")
    artifacts = obj["artifacts"]
    bench_report_artifact = None
    for artifact in artifacts:
        if artifact["name"] == "Runtime Benchmark Report":
            bench_report_artifact = artifact
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
    artifact_ret = _invoke_gh_api(f"/actions/artifacts/{artifact_id}/zip", result_as_text=False)
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


def create_data_for_benchmark(bench_reports: List[JobReport], bench_label: str) -> BenchmarkData:
    """
    Iterates through all the bench_reports and gathers all the data for the given
    bench_label, i.e., for a single benchmark.
    In every JobReport, there is just one score value for a particular `bench_label`.
    That is why we have to iterate all the job reports.
    :param bench_reports All bench reports gathered from the GH API.
    :return:
    """
    entries: List[BenchmarkData.Entry] = []
    for bench_report in bench_reports:
        commit_id = bench_report.bench_run.head_commit.id
        # Not all benchmark labels have to be present in all job reports.
        if bench_label in bench_report.label_score_dict:
            entries.append(BenchmarkData.Entry(
                score=bench_report.label_score_dict[bench_label],
                commit_id=bench_report.bench_run.head_commit.id,
                commit_msg=bench_report.bench_run.head_commit.message,
                commit_url=ENSO_COMMIT_BASE_URL + commit_id,
                commit_author=bench_report.bench_run.head_commit.author.name,
                commit_timestamp=datetime.strptime(bench_report.bench_run.head_commit.timestamp, GH_DATE_FORMAT),
                bench_run_url=bench_report.bench_run.html_url,
                bench_run_event=bench_report.bench_run.event
            ))
    # Sort the entries
    sorted_entries = sorted(entries, key=lambda entry: entry.commit_timestamp)
    return BenchmarkData(bench_label, sorted_entries)


def _label_to_id(label: str) -> str:
    return label.replace(".", "_")


def create_template_data(bench_data: BenchmarkData) -> TemplateBenchData:
    """
    From the given benchmark data, creates data that will be passed into
    the Jinja template.
    """
    logging.debug(f"Creating template data for benchmark with label '{bench_data.label}'")

    def diff_str(score_diff: float, score_diff_perc: float) -> str:
        if not np.isnan(score_diff):
            diff_str = "+" if score_diff > 0 else ""
            diff_str += "{:.5f}".format(score_diff)
            diff_str += " ("
            diff_str += "+" if score_diff_perc > 0 else ""
            diff_str += "{:.5f}".format(score_diff_perc * 100)
            diff_str += "%)"
            return diff_str
        else:
            return "NA"

    # Use pandas to get differences between scores, along with percentual
    # difference.
    score_series = pd.Series((entry.score for entry in bench_data.entries))
    score_diffs = score_series.diff()
    score_diffs_perc = score_series.pct_change()

    # Create rows for each benchmark
    chart_rows: List[ChartRow] = []
    for i in range(len(bench_data.entries)):
        entry = bench_data.entries[i]
        score_diff = score_diffs[i]
        score_diff_perc = score_diffs_perc[i]
        tooltip = "score = " + str(entry.score) + "\\n"
        tooltip += "date = " + str(entry.commit_timestamp) + "\\n"
        tooltip += "diff = " + diff_str(score_diff, score_diff_perc)
        chart_rows.append(
            ChartRow(
                score=entry.score,
                timestamp=entry.commit_timestamp,
                tooltip=tooltip
            )
        )
    score_diffs_str = [diff_str(score_diff, score_diff_perc) for score_diff, score_diff_perc in zip(score_diffs, score_diffs_perc)]
    return TemplateBenchData(
        id=_label_to_id(bench_data.label),
        rows=chart_rows,
        score_diffs=score_diffs_str,
        commit_authors=[entry.commit_author for entry in bench_data.entries],
        commit_ids=[entry.commit_id for entry in bench_data.entries],
        # Take just the first row of a commit message and replace all ' with ".
        commit_msgs=[entry.commit_msg.splitlines()[0].replace("'", "\"") for entry in bench_data.entries],
        commit_urls=[entry.commit_url for entry in bench_data.entries],
        bench_run_urls=[entry.bench_run_url for entry in bench_data.entries],
    )


def render_html(jinja_data: JinjaData, template_file: str, html_out_fname: str) -> None:
    jinja_env = jinja2.Environment(loader=jinja2.FileSystemLoader("."))
    jinja_template = jinja_env.get_template(template_file)
    generated_html = jinja_template.render({
        "since": jinja_data.since,
        "until": jinja_data.until,
        "bench_datas": jinja_data.bench_datas
    })
    with open(html_out_fname, "w") as html_file:
        html_file.write(generated_html)


def compare_runs(bench_run_id_1: str, bench_run_id_2: str, cache: Cache, tmp_dir: str) -> None:
    def perc_str(perc: float) -> str:
        s = "+" if perc > 0 else ""
        s += "{:.5f}".format(perc)
        s += "%"
        return s

    def percent_change(old_val: float, new_val: float) -> float:
        return ((new_val - old_val) / old_val) * 100

    def commit_to_str(commit: Commit) -> str:
        return f"{commit.timestamp} {commit.author.name}  '{commit.message.splitlines()[0]}'"

    res_1 = _invoke_gh_api(f"/actions/runs/{bench_run_id_1}")
    bench_run_1 = _parse_bench_run_from_json(res_1)
    res_2 = _invoke_gh_api(f"/actions/runs/{bench_run_id_2}")
    bench_run_2 = _parse_bench_run_from_json(res_2)
    bench_report_1 = get_bench_report(bench_run_1, cache, tmp_dir)
    bench_report_2 = get_bench_report(bench_run_2, cache, tmp_dir)
    # Check that the runs have the same labels, and get their intersection
    bench_labels_1 = set(bench_report_1.label_score_dict.keys())
    bench_labels_2 = set(bench_report_2.label_score_dict.keys())
    if bench_labels_1 != bench_labels_2:
        logging.warning(
            f"Benchmark labels are not the same in both runs. This means that "
            f"there will be some missing numbers in one of the runs. "
            f"The set difference is {bench_labels_1.difference(bench_labels_2)}")
    all_labels: List[str] = sorted(
        list(bench_labels_1.intersection(bench_labels_2)))
    bench_report_2.label_score_dict.keys()

    df = pd.DataFrame(columns=["bench_label", "score-run-1", "score-run-2"])
    for bench_label in all_labels:
        df = pd.concat([df, pd.DataFrame([{
            "bench_label": bench_label,
            "score-run-1": bench_report_1.label_score_dict[bench_label],
            "score-run-2": bench_report_2.label_score_dict[bench_label],
        }])], ignore_index=True)
    df["score-diff"] = np.diff(df[["score-run-1", "score-run-2"]], axis=1)
    df["score-diff-perc"] = df.apply(lambda row: perc_str(
        percent_change(row["score-run-1"], row["score-run-2"])),
                                     axis=1)
    print("================================")
    print(df.to_string(index=False, header=True, justify="center", float_format="%.5f"))
    print("================================")
    print("Latest commit on bench-run-id-1: ")
    print("    " + commit_to_str(bench_run_1.head_commit))
    print("Latest commit on bench-run-id-2: ")
    print("    " + commit_to_str(bench_run_2.head_commit))


if __name__ == '__main__':
    default_since = datetime.now() - timedelta(days=14)
    default_until = datetime.now()
    default_cache_dir = path.expanduser("~/.cache/enso_bench_download")
    date_format_help = DATE_FORMAT.replace("%", "%%")

    arg_parser = ArgumentParser(description=__doc__,
                                formatter_class=RawDescriptionHelpFormatter)
    arg_parser.add_argument("-s", "--since", action="store",
                            default=default_since,
                            metavar="SINCE_DATE",
                            type=lambda s: datetime.strptime(s, DATE_FORMAT),
                            help=f"The date from which the benchmark results will be gathered. "
                                 f"Format is {date_format_help}. "
                                 f"The default is 14 days before")
    arg_parser.add_argument("-u", "--until", action="store",
                            default=default_until,
                            metavar="UNTIL_DATE",
                            type=lambda s: datetime.strptime(s, DATE_FORMAT),
                            help=f"The date until which the benchmark results will be gathered. "
                                 f"Format is {date_format_help}. "
                                 f"The default is today")
    arg_parser.add_argument("--compare",
                            nargs=2,
                            default=[],
                            metavar=("<Bench action ID 1>", "<Bench action ID 2>"),
                            help="Compare two benchmark actions runs. Choose an action from https://github.com/enso-org/enso/actions/workflows/benchmark.yml, "
                                 "and copy its ID from the URL. For example ID 4602465427 from URL https://github.com/enso-org/enso/actions/runs/4602465427. "
                                 "This option excludes --since, --until, --output, and --create-csv options.")
    arg_parser.add_argument("-o", "--output",
                            default="Engine_Benchs/data/benchs.csv",
                            metavar="CSV_OUTPUT",
                            help="Output CSV file. Makes sense only when used with --create-csv argument")
    arg_parser.add_argument("-c", "--cache", action="store",
                            default=default_cache_dir,
                            metavar="CACHE_DIR",
                            help=f"Cache directory. Makes sense only iff specified with --use-cache argument. "
                                 f"The default is {default_cache_dir}. If there are any troubles with the "
                                 f"cache, just do `rm -rf {default_cache_dir}`.")
    arg_parser.add_argument("-t", "--tmp-dir", action="store",
                            default=None,
                            help="Temporary directory with default created by `tempfile.mkdtemp()`")
    arg_parser.add_argument("--use-cache",
                            default=True,
                            metavar="(true|false)",
                            type=lambda input: True if input in ("true", "True") else False,
                            help="Whether the cache directory should be used. The default is True.")
    arg_parser.add_argument("--create-csv", action="store_true",
                            default=False,
                            help="Whether an intermediate `benchs.csv` should be created. "
                                 "Appropriate to see whether the benchmark downloading was successful. "
                                 "Or if you wish to inspect the CSV with Enso")
    arg_parser.add_argument("-v", "--verbose", action="store_true")
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
    csv_fname: str = args.output
    create_csv: bool = args.create_csv
    compare: List[str] = args.compare
    logging.info(f"parsed args: since={since}, until={until}, cache_dir={cache_dir}, "
                 f"temp_dir={temp_dir}, use_cache={use_cache}, output={csv_fname}, "
                 f"create_csv={create_csv}, compare={compare}")

    if use_cache:
        cache = populate_cache(cache_dir)
    else:
        cache = FakeCache()

    if len(compare) > 0:
        compare_runs(compare[0], compare[1], cache, temp_dir)
    else:
        bench_runs = get_bench_runs(since, until)
        if len(bench_runs) == 0:
            print(
                f"No successful benchmarks found within period since {since} until {until}")
            exit(1)
        job_reports: List[JobReport] = []
        for bench_run in bench_runs:
            job_report = get_bench_report(bench_run, cache, temp_dir)
            if job_report:
                job_reports.append(job_report)
        logging.debug(f"Got {len(job_reports)} job reports")
        if len(job_reports) == 0:
            print("There were 0 job_reports in the specified time interval, so "
                  "there is nothing to visualize or compare.")
            exit(1)

        if create_csv:
            write_bench_reports_to_csv(job_reports, csv_fname)
            logging.info(f"Benchmarks written to {csv_fname}")
            print(f"The generated CSV is in {csv_fname}")

        # Create a separate datatable for each benchmark label
        # with 'label' and 'commit_timestamp' as columns.
        bench_labels: List[str] = list(job_reports[0].label_score_dict.keys())
        template_bench_datas: List[TemplateBenchData] = []
        for bench_label in bench_labels:
            bench_data = create_data_for_benchmark(job_reports, bench_label)
            template_bench_datas.append(create_template_data(bench_data))
        jinja_data = JinjaData(
            since=since,
            until=until,
            bench_datas=template_bench_datas
        )

        # Render Jinja template with jinja_data
        render_html(jinja_data, JINJA_TEMPLATE, "index.html")
        index_html_path = os.path.join(os.getcwd(), "index.html")

        print(f"The generated HTML is in {index_html_path}")
        print(f"Open file://{index_html_path} in the browser")
