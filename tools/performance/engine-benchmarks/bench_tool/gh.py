import asyncio
import base64
import json
import logging
import subprocess
import sys
from typing import Dict, Optional, Union, Any
from urllib.parse import urlencode

_logger = logging.getLogger(__name__)

MAX_BACKOFF_SECONDS = 120


def ensure_gh_installed() -> None:
    try:
        out = subprocess.run(["gh", "--version"], check=True,
                             capture_output=True)
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
    fields: Dict[str, str] = {},
    result_as_json: bool = True,
    method: str = "GET",
    backoff: int = 0,
) -> Optional[Union[Dict[str, Any], bytes]]:
    """
    Invokes the GitHub API using the `gh` command line tool.
    :param repo: Repository name in the form `owner/repo`
    :param endpoint: Endpoint of the query. Must start with `/`.
    :param query_params: Additional query parameters.
    :param fields: Additional fields to be added to the query. add static
    string parameters to the request payload.
    :param result_as_json: If result should be parsed as JSON.
          If false, the raw bytes are returned.
    :param method: HTTP method to use, 'GET' by default.
    :param backoff: Number of seconds to wait before retrying the request.
    If higher than 0, it means that the request has already been retried,
    try to do it again, with a higher backoff.
    :return: None if the query fails
    """
    assert endpoint.startswith("/")
    if len(fields) > 0 and method != "POST":
        raise ValueError("Fields can be used only with POST method")
    urlencode(query_params)
    cmd = [
        "gh",
        "api",
        "--method", method,
        f"/repos/{repo}{endpoint}" + "?" + urlencode(query_params)
    ]
    for k, v in fields.items():
        cmd.append("-f")
        cmd.append(f"{k}='{v}'")
    if 0 < backoff <= MAX_BACKOFF_SECONDS:
        _logger.debug(f"Backing off for {backoff} seconds")
        await asyncio.sleep(backoff)
    elif backoff > MAX_BACKOFF_SECONDS:
        _logger.error(f"Backoff of {backoff} seconds is too high, giving up.")
        return None
    _logger.debug("Invoking gh API with `%s`", " ".join(cmd))
    proc = await asyncio.create_subprocess_exec("gh", *cmd[1:],
                                                stdout=subprocess.PIPE,
                                                stderr=subprocess.PIPE)
    out, err = await proc.communicate()
    _logger.debug("Finished gh API `%s`", " ".join(cmd))
    if proc.returncode != 0:
        # Special handling of rate limit exceeded - just try to make the
        # request one more time after some backoff.
        if "You have exceeded a secondary rate limit" in err.decode():
            new_backoff = 10 if backoff == 0 else backoff * 2
            _logger.warning(f"Trying to retry the request with a new backoff "
                            f"of {new_backoff} seconds.")
            return await invoke_gh_api(repo, endpoint, query_params, fields,
                                       result_as_json, method, new_backoff)
        else:
            _logger.error("Command `%s` FAILED with errcode %d",
                          " ".join(cmd),
                          proc.returncode)
            _logger.error("  stdout: %s", out.decode())
            _logger.error("  stderr: %s", err.decode())
            return None
    if result_as_json:
        return json.loads(out.decode())
    else:
        return out


async def fetch_file(repo: str, file_path: str) -> Optional[str]:
    ret = await invoke_gh_api(repo, f"/contents/{file_path}",
                              result_as_json=True)
    if ret is None:
        _logger.warning("File %s not found in %s", file_path, repo)
        return None
    file_content = base64.b64decode(ret["content"]).decode()
    return file_content
