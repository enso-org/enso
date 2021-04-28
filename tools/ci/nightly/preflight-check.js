const fs = require("fs");
const github = require("./github");

const currentHeadSha = process.argv[2];
const buildConfigPath = "../../../build.sbt";

/// Returns the current date formatted as 'YYYY-mm-dd'.
function isoDate() {
  const now = new Date();
  const year = "" + now.getFullYear();
  let month = "" + (now.getMonth() + 1);
  let day = "" + now.getDate();
  if (month.length < 2) {
    month = "0" + month;
  }
  if (day.length < 2) {
    day = "0" + day;
  }
  return year + "-" + month + "-" + day;
}

/// Sets the step output 'proceed'.
function setProceed(proceed) {
  console.log("::set-output name=proceed::" + proceed);
}

/// Sets the step output 'nightly-version'.
function setVersionString(name) {
  console.log("::set-output name=nightly-version::" + name);
}

/** Checks if there are any new changes to see if the nightly build should
 * proceed.
 */
function checkProceed(nightlies) {
  if (nightlies.length == 0) {
    console.log(
      "No prior nightly releases found. Proceeding with the first release."
    );
    return true;
  }

  const first = nightlies[0];
  const firstNightlySha = first.target_commitish;
  if (firstNightlySha == currentHeadSha) {
    console.log(
      "Current commit (" +
        currentHeadSha +
        ") is the same as for the most recent nightly build. A new build is not needed."
    );
    return false;
  } else {
    console.log(
      "Current commit (" +
        currentHeadSha +
        ") is different from the most recent nightly build (" +
        firstNightlySha +
        "). Proceeding with a new nightly build."
    );
    return true;
  }
}

/** Prepares a version string for the nightly build.
 *
 * A '-SNAPSHOT' suffix is added if it is not already present, next the current
 * date is appended. If this is not the first nightly build on that date, an
 * increasing numeric suffix is added.
 */
function prepareVersionString(nightlies) {
  function isTaken(name) {
    const tagName = "enso-" + name;
    return nightlies.some((entry) => entry.tag_name == tagName);
  }

  const content = fs.readFileSync(buildConfigPath, { encoding: "utf-8" });
  const match = content.match(/val ensoVersion += +"(.*)"/);
  if (!match) {
    console.error("Could not find the version string in configuration!");
    process.exit(1);
  }

  const version = match[1];
  let baseName = version;
  if (!baseName.endsWith("SNAPSHOT")) {
    baseName += "-SNAPSHOT";
  }

  baseName += "." + isoDate();

  function makeName(ix) {
    if (ix == 0) {
      return baseName;
    } else {
      return baseName + "." + ix;
    }
  }

  let ix = 0;
  while (isTaken(makeName(ix))) {
    ix++;
  }

  const name = makeName(ix);
  console.log("The build will be using version '" + name + "'");
  return name;
}

async function main() {
  const nightlies = await github.fetchNightlies();
  const shouldProceed = checkProceed(nightlies);
  setProceed(shouldProceed);
  if (shouldProceed) {
    const versionString = prepareVersionString(nightlies);
    setVersionString(versionString);
  }
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
