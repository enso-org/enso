
import { Octokit, App } from "octokit";

const ghToken = process.env.GITHUB_TOKEN;

const octoKit = new Octokit({
    auth: ghToken
});
