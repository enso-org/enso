import asyncio
import logging
import math
import os
import shutil
import zipfile
from datetime import datetime
from os import path
from typing import List, Dict, Optional, Any
from xml.etree import ElementTree as ET

from bench_tool import JobRun, DATE_FORMAT, ENSO_REPO, JobReport, Source
from bench_tool.gh import invoke_gh_api
from bench_tool.remote_cache import RemoteCache
from bench_tool.utils import WithTempDir

ARTIFACT_ID = "Runtime Benchmark Report"

_logger = logging.getLogger(__name__)


async def get_bench_runs(since: datetime, until: datetime, branch: str, workflow_id: int) -> List[JobRun]:
    """
    Fetches the list of all the SUCCESSFUL job runs from the GH API for the specified `branch`.

    :param since: The date from which the benchmark results will be gathered.
    :param until: The date until which the benchmark results will be gathered.
    :param branch: The branch for which the benchmark results will be gathered.
    :param workflow_id: The ID of the workflow for which the benchmark results will be gathered.
    """
    _logger.info(f"Looking for all successful Engine benchmark workflow run "
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
    _logger.debug(f"Total count of all runs: {total_count} for workflow ID "
                  f"{workflow_id}. Will process {per_page} runs per page")

    async def get_and_parse_run(page: int, parsed_bench_runs) -> None:
        _query_fields = query_fields.copy()
        _query_fields["page"] = str(page)
        res = await invoke_gh_api(ENSO_REPO, f"/actions/workflows/{workflow_id}/runs", _query_fields)
        bench_runs_json = res["workflow_runs"]
        _parsed_bench_runs = [JobRun.from_dict(bench_run_json)
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


async def fetch_job_reports(
        bench_runs: List[JobRun],
        remote_cache: RemoteCache
) -> List[JobReport]:
    """
    Fetches all benchmark reports for the given benchmark runs. Benchmark runs are basically
    just IDs of artifacts, and the reports are the actual benchmark results. These results are
    either on the GH as artifacts, or are fetched from the cache if the artifact is expired.
    All the runs are fetched in parallel.
    :param bench_runs:
    :param remote_cache:
    :return:
    """
    job_reports: List[JobReport] = []

    async def _process_report(_bench_run: JobRun):
        with WithTempDir("bench_download") as temp_dir:
            _job_report = await get_bench_report(_bench_run, temp_dir, remote_cache)
        if _job_report:
            job_reports.append(_job_report)

    tasks = []
    for bench_run in bench_runs:
        tasks.append(_process_report(bench_run))
    await asyncio.gather(*tasks)
    return job_reports


def _known_artifact_names() -> List[str]:
    return Source.STDLIB.artifact_names() + Source.ENGINE.artifact_names()


async def get_bench_report(bench_run: JobRun, temp_dir: str, remote_cache: RemoteCache) -> Optional[JobReport]:
    """
    Extracts some data from the given bench_run, which was fetched via the GH API,
    optionally getting it from the cache.
    An artifact in GH can expire, in such case, returns None.
    :param bench_run:
    :param temp_dir: Used for downloading and unzipping artifacts.
    :return: None if the corresponding artifact cannot be found, neither as a GH artifact, neither from the remote cache.
    """
    assert os.path.exists(temp_dir) and os.path.isdir(temp_dir)

    # There might be multiple artifacts in the artifact list for a benchmark run
    # We are looking for the one named 'Runtime Benchmark Report', which will
    # be downloaded as a ZIP file.
    obj: Dict[str, Any] = await invoke_gh_api(ENSO_REPO, f"/actions/runs/{bench_run.id}/artifacts")
    artifacts = obj["artifacts"]
    artifacts_by_names = {artifact["name"]: artifact for artifact in artifacts}
    # At this point, we don't know the source of the benchmark - either it is from
    # Engine, or from stdlib. Thus, we don't know exactly which artifact name we
    # are looking for. But we know, there must be exactly one of the artifact names.
    bench_report_artifact = None
    for known_name in _known_artifact_names():
        if known_name in artifacts_by_names:
            bench_report_artifact = artifacts_by_names[known_name]
    if bench_report_artifact is None:
        _logger.warning(f"Bench run {bench_run.id} does not contain any of the known artifact names: "
                        f"{_known_artifact_names()}, but it is a successful run.")
        return None
    assert bench_report_artifact, "Benchmark Report artifact not found"
    artifact_id = str(bench_report_artifact["id"])
    created_at = bench_report_artifact["created_at"]
    updated_at = bench_report_artifact["updated_at"]
    expires_at = bench_report_artifact["expires_at"]
    is_expired = bench_report_artifact["expired"]
    _logger.debug(f"Got artifact with ID {artifact_id}, from bench run {bench_run.id}: "
                  f"created_at={created_at}, updated_at={updated_at}, expires_at={expires_at}, "
                  f"is_expired={is_expired}")

    job_report = await remote_cache.fetch(bench_run.id)
    if is_expired and job_report is None:
        _logger.error(
            f"Artifact {artifact_id} from bench run {bench_run.id} is expired, and it is not in the remote cache")
        return None
    if job_report:
        _logger.debug(f"Got job report from the cache for {bench_run.id}")
        return job_report

    assert not is_expired

    # Get contents of the ZIP artifact file
    artifact_ret = await invoke_gh_api(ENSO_REPO, f"/actions/artifacts/{artifact_id}/zip", result_as_json=False)
    zip_file_name = os.path.join(temp_dir, artifact_id + ".zip")
    _logger.debug(f"Writing artifact ZIP content into {zip_file_name}")
    with open(zip_file_name, "wb") as zip_file:
        zip_file.write(artifact_ret)

    extracted_dirname = os.path.join(temp_dir, artifact_id)
    if os.path.exists(extracted_dirname):
        shutil.rmtree(extracted_dirname)
    os.mkdir(extracted_dirname)

    _logger.debug(f"Extracting {zip_file_name} into {extracted_dirname}")
    zip_file = zipfile.ZipFile(zip_file_name, "r")
    zip_file.extractall(extracted_dirname)
    bench_report_xml = path.join(extracted_dirname, "bench-report.xml")
    assert path.exists(bench_report_xml)

    bench_report_parsed = _parse_bench_report_from_xml(bench_report_xml, bench_run)
    await remote_cache.put(bench_run.id, bench_report_parsed)
    return bench_report_parsed


def _parse_bench_report_from_xml(bench_report_xml_path: str, bench_run: JobRun) -> "JobReport":
    _logger.debug(f"Parsing BenchReport from {bench_report_xml_path}")
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
                _logger.warning(f"More than one score for benchmark {label}, "
                                f"using the last one (the newest one).")
            label_score_dict[label] = scores_float[len(scores_float) - 1]
    return JobReport(
        label_score_dict=label_score_dict,
        bench_run=bench_run
    )

