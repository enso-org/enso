
import { Octokit, App } from "octokit";
import { program } from "commander";
import { readFileSync, existsSync } from "fs";


function findGHToken() {
    let ghToken = process.env.GITHUB_TOKEN;
    if (!ghToken) {
      // Read $HOME/.github-token
      const ghTokenPath = `${process.env.HOME}/GITHUB_TOKEN`;
      if (existsSync(ghTokenPath)) {
        ghToken = readFileSync(ghTokenPath, "utf-8")
      }
    }
    if (!ghToken) {
        console.error("GITHUB_TOKEN not found in env vars neither in ~/GITHUB_TOKEN file");
        process.exit(1);
    }
    return ghToken;
}

function main() {
  program
    .name("bench-data")
    .description("Utility to fetch benchmarking data from GH Actions")
    .option("-v", "--verbose", "Enable verbose logging")

  program.parse();

  const options = program.opts();
  console.log("options", options);

  const ghToken = findGHToken();
  console.log("ghToken", ghToken);
  const octoKit = new Octokit({
      auth: ghToken
  });
  console.log("octoKit", octoKit);
}

main();
