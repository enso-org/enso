const github = require("./github");

const releaseId = process.argv[2];

async function main() {
  console.log("Making release " + releaseId + " public.");
  github.publishRelease(releaseId);
  console.log("Done.")
}

main();
