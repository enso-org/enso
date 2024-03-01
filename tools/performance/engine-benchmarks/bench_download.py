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

from bench_tool.bench_results import get_bench_runs, fetch_job_reports
from bench_tool.remote_cache import ReadonlyRemoteCache
from bench_tool.utils import gather_all_bench_labels, sort_job_reports

if not (sys.version_info.major >= 3 and sys.version_info.minor >= 7):
    print("ERROR: python version lower than 3.7")
    exit(1)

import asyncio
import logging
import logging.config
import os
import shutil
import tempfile
from argparse import ArgumentParser, RawDescriptionHelpFormatter
from csv import DictWriter
from datetime import datetime, timedelta
from os import path
from typing import List, Dict, Optional, Set

from bench_tool import DATE_FORMAT, GENERATED_SITE_DIR, GH_ARTIFACT_RETENTION_PERIOD, TEMPLATES_DIR, \
    JINJA_TEMPLATE, JobRun, JobReport, \
    TemplateBenchData, JinjaData, Source
from bench_tool.gh import ensure_gh_installed
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

        job_reports = await fetch_job_reports(bench_runs, remote_cache)
        logging.debug(f"Got {len(job_reports)} job reports for branch {branch}")
        if len(job_reports) == 0:
            print(f"There were 0 job_reports in the specified time interval, "
                  f"for branch {branch}, so "
                  "there is nothing to visualize or compare.")
            exit(1)

        logging.debug("Sorting job_reports by commit date")
        sort_job_reports(job_reports)

        if create_csv:
            write_bench_reports_to_csv(job_reports, csv_output)
            logging.info(f"Benchmarks written to {csv_output}")
            print(f"The generated CSV is in {csv_output}")
            exit(0)

        # Gather all the benchmark labels from all the job reports
        if bench_labels is None:
            all_bench_labels = gather_all_bench_labels(job_reports)
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
        timestamp=datetime.now()
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
