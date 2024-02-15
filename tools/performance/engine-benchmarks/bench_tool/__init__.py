import os
from datetime import timedelta
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
