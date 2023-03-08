#!/usr/bin/python

"""
Script for downloading Engine benchmark results into a CSV file.
Without any options, downloads benchmark data for the last 14 days and stores
them into `data/benchs.csv`.
It downloads the data synchronously and uses a cache directory.
Cache is not used by default, enable it with `--use-cache`, but it may not work properly.
It is advised to use `-v|--verbose` option all the time.

For description of the options, just use `-h`.

It queries only successful benchmark runs. If there are no successful benchmarks
in a given period, no results will be written.
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
from argparse import ArgumentParser
from csv import DictWriter
from datetime import datetime, timedelta
from os import path
from typing import List, Dict, Optional, Any, Union

from dataclasses import dataclass

BENCH_RUN_NAME = "Benchmark Engine"
DATE_FORMAT = "%Y-%m-%d"
# Workflod ID of engine benchmarks, got via `gh api '/repos/enso-org/enso/actions/workflows'`
BENCH_WORKFLOW_ID = 29450898


@dataclass
class Author:
    name: str


@dataclass
class Commit:
    id: str
    author: Author
    timestamp: str
    message: str


@dataclass
class BenchRun:
    id: str
    display_title: str
    html_url: str
    run_attempt: int
    event: str
    head_commit: Commit


@dataclass
class BenchReport:
    label_score_dict: Dict[str, float]
    bench_run: BenchRun


def _parse_bench_run_from_json(obj: Dict[Any, Any]) -> BenchRun:
    return BenchRun(
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


def _parse_bench_report_from_json(obj: Dict[Any, Any]) -> BenchReport:
    return BenchReport(
        bench_run=_parse_bench_run_from_json(obj["bench_run"]),
        label_score_dict=obj["label_score_dict"]
    )


def _bench_report_to_json(bench_report: BenchReport) -> Dict[Any, Any]:
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


def _parse_bench_report_from_xml(bench_report_xml: str, bench_run: BenchRun) -> "BenchReport":
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
    return BenchReport(
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
    ret = subprocess.run(cmd, check=True, text=result_as_text, capture_output=True)
    if result_as_text:
        return json.loads(ret.stdout)
    else:
        return ret.stdout


class Cache:
    """
    Cache is a directory filled with json files that have name of format <bench_run_id>.json, and
    in every json, there is `BenchReport` dataclass serialized.
    """
    def __init__(self, dirname: str):
        assert path.exists(dirname) and path.isdir(dirname)
        self._dir = dirname
        # Keys are BenchRun ids
        self._items: Dict[str, BenchReport] = {}
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

    def __getitem__(self, item: str) -> Optional[BenchReport]:
        if not _is_benchrun_id(item):
            return None
        else:
            return self._items[item]

    def __setitem__(self, bench_run_id: str, bench_report: BenchReport) -> None:
        assert isinstance(bench_report, BenchReport)
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


def get_bench_runs(since: datetime, until: datetime) -> List[BenchRun]:
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


def get_bench_report(bench_run: BenchRun, cache: Cache, temp_dir: str) -> BenchReport:
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


def write_bench_reports_to_csv(bench_reports: List[BenchReport], csv_fname: str) -> None:
    logging.info(f"Writing {len(bench_reports)} benchmark reports to {csv_fname}")
    assert len(bench_reports) > 0
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


if __name__ == '__main__':
    default_since = datetime.now() - timedelta(days=14)
    default_until = datetime.now()

    arg_parser = ArgumentParser("enso_bench")
    arg_parser.add_argument("-s", "--since", action="store",
                            default=default_since,
                            type=lambda s: datetime.strptime(s, DATE_FORMAT),
                            help=f"The date from which the benchmark results will be gathered. Format is {DATE_FORMAT}."
                                 f"The default is 14 days before")
    arg_parser.add_argument("-u", "--until", action="store",
                            default=default_until,
                            type=lambda s: datetime.strptime(s, "%Y-%m-%d"),
                            help=f"The date until which the benchmark results will be gathered. Format is {DATE_FORMAT}"
                                 f"The default is today")
    arg_parser.add_argument("-o", "--output",
                            default="Engine_Benchs/data/benchs.csv",
                            help="Output CSV file")
    arg_parser.add_argument("-c", "--cache", action="store",
                            default=path.expanduser("~/.cache/enso_bench_cmp"),
                            help="Cache directory. Makes sense only iff specified with --use-cache argument")
    arg_parser.add_argument("-t", "--tmp-dir", action="store",
                            default=None,
                            help="Temporary directory with default created by `tempfile.mkdtemp()`")
    arg_parser.add_argument("--use-cache", action="store_true",
                            default=False,
                            help="Whether the cache directory should be used")
    arg_parser.add_argument("-v", "--verbose", action="store_true")
    args = arg_parser.parse_args()
    if args.verbose:
        log_level = logging.DEBUG
    else:
        log_level = logging.INFO
    logging.basicConfig(level=log_level, stream=sys.stdout)
    if not args.since:
        logging.error("--since option not specified")
        arg_parser.print_help()
        exit(1)
    since: datetime = args.since
    if not args.until:
        logging.error(f"--until option not specified")
        arg_parser.print_help()
        exit(1)
    until: datetime = args.until
    cache_dir: str = args.cache
    if not args.tmp_dir:
        temp_dir: str = tempfile.mkdtemp()
    else:
        temp_dir: str = args.tmp_dir
    use_cache: bool = args.use_cache
    assert cache_dir and temp_dir
    csv_fname: str = args.output
    logging.info(f"parsed args: since={since}, until={until}, cache_dir={cache_dir}, "
                 f"temp_dir={temp_dir}, use_cache={use_cache}, output={csv_fname}")

    if use_cache:
        cache = populate_cache(cache_dir)
    else:
        cache = FakeCache()

    bench_runs = get_bench_runs(since, until)
    if len(bench_runs) > 0:
        bench_reports = [get_bench_report(bench_run, cache, temp_dir) for bench_run in bench_runs]
        write_bench_reports_to_csv(bench_reports, csv_fname)
        print(f"Benchmarks written to {csv_fname}")
    else:
        print(f"No successful benchmarks found within period since {since} until {until}")
        exit(1)

