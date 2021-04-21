const github = require("./github");

const currentHeadSha = process.argv[2];

function setProceed(proceed) {
  console.log("::set-output name=proceed::" + proceed)
}

async function main() {
  const nightlies = await github.fetchNightlies();
  if (nightlies.length == 0) {
    console.log("No prior nightly releases found. Proceeding with the first release.")
    setProceed(true);
    return;
  }

  const first = nightlies[0];
  const firstNightlySha = first.target_commitish;
  if (firstNightlySha == currentHeadSha) {
    console.log("Current commit (" + currentHeadSha + ") is the same as for the most recent nightly build. A new build is not needed.")
    setProceed(false);
  } else {
    console.log("Current commit (" + currentHeadSha + ") is different from the most recent nightly build (" + firstNightlySha + "). Proceeding with a new nightly build.")
    setProceed(true);
  }
}

main().catch(err => {
  console.error(err);
  process.exit(1);
});
