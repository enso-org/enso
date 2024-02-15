import os
from dataclasses import dataclass
from datetime import timedelta, datetime
from enum import Enum
from os import path
from typing import List, Dict, Optional, Set, Tuple
import re
import subprocess
import sys
from argparse import ArgumentParser
import shutil
from pathlib import Path


def pkg_dir() -> Path:
    """ Directory of this package """
    return Path(os.path.dirname(os.path.realpath(__file__)))


DATE_FORMAT = "%Y-%m-%d"
GH_DATE_FORMAT = "%Y-%m-%dT%H:%M:%SZ"
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
""" Date format as returned from responses in GH API"""
ENSO_COMMIT_BASE_URL = "https://github.com/enso-org/enso/commit/"

GH_ARTIFACT_RETENTION_PERIOD = timedelta(days=90)

GENERATED_SITE_DIR = pkg_dir().parent.joinpath("generated_site")
TEMPLATES_DIR = pkg_dir().parent.joinpath("templates")
JINJA_TEMPLATE = TEMPLATES_DIR.joinpath("template_jinja.html")

assert TEMPLATES_DIR.exists()
assert JINJA_TEMPLATE.exists()


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



