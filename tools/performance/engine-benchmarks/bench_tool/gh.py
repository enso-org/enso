import base64
import json
import logging
import os
from os import path
from typing import List, Dict, Optional, Set, Tuple, Union, Any
import re
import subprocess
import sys
from argparse import ArgumentParser
import shutil
import asyncio
from urllib.parse import urlencode

_logger = logging.getLogger(__name__)


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


async def invoke_gh_api(
        repo: str,
        endpoint: str,
        query_params: Dict[str, str] = {},
        result_as_json: bool = True
) -> Union[Dict[str, Any], bytes]:
    """
    Invokes the GitHub API using the `gh` command line tool.
    :param repo: Repository name in the form `owner/repo`
    :param endpoint: Endpoint of the query. Must start with `/`.
    :param query_params: Additional query parameters.
    :param result_as_json: If result should be parsed as JSON.
          If false, the raw bytes are returned.
    :return:
    """
    assert endpoint.startswith("/")
    urlencode(query_params)
    cmd = [
        "gh",
        "api",
        f"/repos/{repo}{endpoint}" + "?" + urlencode(query_params)
    ]
    _logger.debug("Invoking gh API with `%s`", " ".join(cmd))
    proc = await asyncio.create_subprocess_exec("gh", *cmd[1:],
                                                stdout=subprocess.PIPE,
                                                stderr=subprocess.PIPE)
    out, err = await proc.communicate()
    _logger.debug("Finished gh API `%s`", " ".join(cmd))
    if proc.returncode != 0:
        _logger.error("Command `%s` FAILED with errcode %d",
                      " ".join(cmd),
                      proc.returncode)
        exit(proc.returncode)
    if result_as_json:
        return json.loads(out.decode())
    else:
        return out


async def fetch_file(repo: str, file_path: str) -> Optional[str]:
    ret = await invoke_gh_api(repo, f"/contents/{file_path}", result_as_json=True)
    if ret is None:
        _logger.warning("File %s not found in %s", file_path, repo)
        return None
    file_content = base64.b64decode(ret["content"]).decode()
    return file_content
