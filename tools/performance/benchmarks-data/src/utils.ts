import { readFileSync, existsSync } from 'fs'
import { join } from 'path'

export function findRepoRoot(): string {
    let currentDir = process.cwd()
    while (currentDir !== '/') {
        if (existsSync(join(currentDir, ".git"))) {
            return currentDir;
        }
        currentDir = join(currentDir, '..')
    }
    console.error('Could not find repo root directory');
    process.exit(1);
}

export function findGHToken(): string {
    let ghToken = process.env.GITHUB_TOKEN
    if (!ghToken) {
        if (process.env.HOME) {
            const ghTokenPath = join(process.env.HOME, "GITHUB_TOKEN")
            if (existsSync(ghTokenPath)) {
                ghToken = readFileSync(ghTokenPath, 'utf-8')
            }
        }
    }
    if (!ghToken) {
        console.error('GITHUB_TOKEN not found in env vars neither in ~/GITHUB_TOKEN file')
        process.exit(1)
    }
    return ghToken
}
