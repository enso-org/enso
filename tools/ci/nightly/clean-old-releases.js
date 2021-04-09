const github = require("./github");

const MAX_NIGHTLY_RELEASES_TO_KEEP = 3;

function setProceed(proceed) {
  console.log("::set-output name=proceed::" + proceed)
}

async function main() {
  const nightlies = await github.fetchNightlies();
  if (nightlies.length <= MAX_NIGHTLY_RELEASES_TO_KEEP) {
    console.log("No releases to clean.")
    return;
  }

  const toRemoveCount = nightlies.length - MAX_NIGHTLY_RELEASES_TO_KEEP;
  if (toRemoveCount > 5) {
    console.log("There are " + toRemoveCount + " not-cleaned releases, but every time one is added, older ones are cleaned, so normally there should be at most one to remove. This is very suspicious and it may indicate that the selected set of releases is too broad. Cleaning has been abandoned and the situation should be investigated manually.")
    return;
  }

  for (let i = MAX_NIGHTLY_RELEASES_TO_KEEP; i < nightlies.length; ++i) {
    const id = nightlies[i].id;
    console.log("Removing old release " + id + ".")
    await github.removeRelease(id);
  }

  console.log("Cleanup complete.")
}

main();
