const fs = require('fs');
const proc = require('child_process');

const changelogPath = process.argv[2];
const ref = process.argv[3];

const diffArgs = ["--no-pager", "diff", "--exit-code", ref, changelogPath];
const exitCode = proc.spawnSync("git", diffArgs).status;

if (exitCode == 0) {
   console.log("No changes to the changelog");
   process.exit(1);
} else {
   console.log("Changelog was changed");
   process.exit(0);
}

