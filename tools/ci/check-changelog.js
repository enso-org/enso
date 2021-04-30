const fs = require("fs");
const proc = require("child_process");

const skipChangelogInfix = "[no-changelog]";
const changelogPath = process.argv[2];
const baseRef = process.argv[3];

/// Runs the git command with the provided arguments.
function runGit(args) {
  const result = proc.spawnSync("git", args);
  if (result.error) {
    console.log("Cannot access git", result.error);
    process.exit(1);
  }
  return result;
}

/** Checks if the changelog file was changed in any commits that are part of the
 * PR.
 */
function wasChangelogModified() {
  const diffArgs = [
    "--no-pager",
    "diff",
    "--exit-code",
    baseRef,
    "--",
    changelogPath,
  ];

  const result = runGit(diffArgs);
  const exitCode = result.status;
  console.log(result.stdout.toString("utf-8"));
  const noDifference = exitCode == 0;
  return !noDifference;
}

/// Checks if any commit has overridden the changelog check.
function isChangelogSkipped() {
  const logArgs = ["--no-pager", "log", "HEAD~3...HEAD", "--pretty=oneline"];
  const result = runGit(logArgs);

  const output = result.stdout.toString("utf-8");
  const containsSkipCommit = output.indexOf(skipChangelogInfix) >= 0;
  return containsSkipCommit;
}

if (wasChangelogModified()) {
  console.log("Changelog was changed");
  process.exit(0);
} else {
  console.log("No changes to the changelog");
  if (isChangelogSkipped()) {
    console.log(
      "But one of the commits within the PR includes " +
        skipChangelogInfix +
        ", so the check is skipped."
    );
    process.exit(0);
  } else {
    process.exit(1);
  }
}
