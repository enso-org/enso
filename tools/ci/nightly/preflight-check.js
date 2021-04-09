const { Octokit } = require("@octokit/core");
const octokit = new Octokit();

const currentHeadSha = process.argv[2];
const token = process.env.GITHUB_TOKEN;

const graphqlWithAuth = octokit.graphql.defaults({
  headers: {
    authorization: "token " + token,
  },
});

function setProceed(proceed) {
  console.log("::set-output name=proceed::" + proceed)
}

function isNightly(release) {
  const nightlyInfix = "-SNAPSHOT-";
  return release.tagName.indexOf(nightlyInfix) >= 0;
}

async function main() {
  const query = `
  {
    repository(owner: "enso-org", name: "enso") {
      releases(first: 100, orderBy: {direction: DESC, field: CREATED_AT}) {
        nodes {
          tagName
          tagCommit {
            oid
          }
        }
      }
    }
  }
  `;
  const { repository } = await graphqlWithAuth(query);
  const nightlies = repository.releases.nodes.filter(isNightly);
  if (nightlies.length == 0) {
    console.log("No prior nightly releases found. Proceeding with the first release.")
    setProceed(true);
    return;
  }

  const first = nightlies[0];
  const firstNightlySha = first.tagCommit.oid;
  if (firstNightlySha == currentHeadSha) {
    console.log("Current commit (" + currentHeadSha + ") is the same as for the most recent nightly build. A new build is not needed.")
    setProceed(false);
  } else {
    console.log("Current commit (" + currentHeadSha + ") is different from the most recent nightly build (" + firstNightlySha + "). Proceeding with a new nightly build.")
    setProceed(true);
  }
}

main();
