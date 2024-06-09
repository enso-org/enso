import os
from dataclasses import dataclass
from datetime import timedelta, datetime
from enum import Enum
from pathlib import Path
from typing import List, Dict, Any


def pkg_dir() -> Path:
    """ Directory of this package """
    return Path(os.path.dirname(os.path.realpath(__file__)))


ENSO_REPO = "enso-org/enso"
BENCH_REPO = "enso-org/engine-benchmark-results"
BRANCH_DEVELOP = "develop"
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

    def artifact_names(self) -> List[str]:
        if self == Source.ENGINE:
            return ["Runtime Benchmark Report"]
        elif self == Source.STDLIB:
            return ["Enso JMH Benchmark Report"]
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

    @staticmethod
    def from_dict(obj: Dict[Any, Any]) -> "JobRun":
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

    def to_dict(self) -> Dict[Any, Any]:
        return {
            "id": self.id,
            "html_url": self.html_url,
            "run_attempt": self.run_attempt,
            "event": self.event,
            "display_title": self.display_title,
            "head_commit": {
                "id": self.head_commit.id,
                "message": self.head_commit.message,
                "timestamp": self.head_commit.timestamp,
                "author": {
                    "name": self.head_commit.author.name
                }
            }
        }


@dataclass
class JobReport:
    """
    Gathered via the GH API - a report that is pushed as an aritfact to the job.
    Contains a XML file with scores for all the benchmarks.
    """
    label_score_dict: Dict[str, float]
    """ A mapping of benchmark labels to their scores """
    bench_run: JobRun

    @staticmethod
    def from_dict(obj: Dict[Any, Any]) -> "JobReport":
        return JobReport(
            bench_run=JobRun.from_dict(obj["bench_run"]),
            label_score_dict=obj["label_score_dict"]
        )

    def to_dict(self) -> Dict[Any, Any]:
        return {
            "bench_run": self.bench_run.to_dict(),
            "label_score_dict": self.label_score_dict
        }


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
    timestamp: datetime
    """ The time when the website was generated """



