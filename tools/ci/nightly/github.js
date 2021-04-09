const { Octokit } = require("@octokit/core");
const octokit = new Octokit();

const organization = "enso-org";
const repo = "enso";
const token = process.env.GITHUB_TOKEN;

const graphqlWithAuth = octokit.graphql.defaults({
  headers: {
    authorization: "token " + token,
  },
});

function isNightly(release) {
  const nightlyInfix = "Nightly";
  return release.name.indexOf(nightlyInfix) >= 0;
}

async function fetchAllReleases() {
  const query = `
  query getNightlies($owner: String!, $repo: String!) {
    repository(owner: $owner, name: $repo) {
      releases(first: 100, orderBy: {direction: DESC, field: CREATED_AT}) {
        nodes {
          tagCommit {
            oid
          }
          name
          id
        }
      }
    }
  }
  `;
  const { repository } = await graphqlWithAuth(query, {owner: organization, repo: repo});
  return repository.releases.nodes;
}

async function fetchNightlies() {
  const releases = await fetchAllReleases();
  const nightlies = releases.filter(isNightly);
  return nightlies;
}

async function removeRelease(id) {
  await octokit.request('DELETE /repos/{owner}/{repo}/releases/{release_id}', {
    owner: organization,
    repo: repo,
    release_id: id
  });
}

async function publishRelease(id) {
  await octokit.request('PATCH /repos/{owner}/{repo}/releases/{release_id}', {
    owner: organization,
    repo: repo,
    release_id: id,
    draft: false
  });
}

exports.fetchAllReleases = fetchAllReleases;
exports.fetchNightlies = fetchNightlies;
exports.removeRelease = removeRelease;
exports.publishRelease = publishRelease;
