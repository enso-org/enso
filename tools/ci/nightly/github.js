const { Octokit } = require("@octokit/core");

const organization = "enso-org";
function determineRepositoryName() {
  const fallback = "enso";
  const fallbackMessage =
    "Could not determine the repository name, falling back to the default.";
  const fullName = process.env.GITHUB_REPOSITORY;
  if (!fullName) {
    console.log(fallbackMessage);
    return fallback;
  }

  const prefix = organization + "/";
  if (fullName.startsWith(prefix)) {
    return fullName.substring(prefix.length);
  } else {
    console.log(fallbackMessage);
    return fallback;
  }
}

const repo = determineRepositoryName();
const token = process.env.GITHUB_TOKEN;
const octokit = new Octokit({ auth: token });

function isNightly(release) {
  const nightlyInfix = "Nightly";
  return release.name.indexOf(nightlyInfix) >= 0 && !release.draft;
}

async function fetchAllReleases() {
  const res = await octokit.request("GET /repos/{owner}/{repo}/releases", {
    owner: organization,
    repo: repo,
  });
  return res.data;
}

async function fetchNightlies() {
  const releases = await fetchAllReleases();
  const nightlies = releases.filter(isNightly);
  return nightlies;
}

async function triggerWorkflow(repo, workflow_id, ref) {
  await octokit.request(
    "POST /repos/{owner}/{repo}/actions/workflows/{workflow_id}/dispatches",
    {
      owner: organization,
      repo: repo,
      workflow_id: workflow_id,
      ref: ref,
    }
  );
}

async function publishRelease(id) {
  return await octokit.request(
    "PATCH /repos/{owner}/{repo}/releases/{release_id}",
    {
      owner: organization,
      repo: repo,
      release_id: id,
      draft: false,
    }
  );
}

exports.fetchAllReleases = fetchAllReleases;
exports.fetchNightlies = fetchNightlies;
exports.publishRelease = publishRelease;
exports.repository = repo;
