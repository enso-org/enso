import esbuild from 'esbuild'

// ===================
// === Arg Parsing ===
// ===================

import { parseArgs } from 'node:util'
import child_process from 'node:child_process'

let out = parseArgs({
    options: {
        outfile: {
            type: 'string',
        },
    },
})

let outfile = out.values.outfile
if (!outfile) {
    console.error('No --outfile option provided.')
    process.exit(1)
}

// ===================
// === Git Process ===
// ===================

/**
 * Get output of a git command.
 * @param command Command line following the `git` program.
 * @returns Output of the command.
 */
function git(command: string): string {
    return child_process.execSync(`git ${command}`, { encoding: 'utf8' }).trim()
}

// ====================
// === Build Config ===
// ====================

const config: esbuild.BuildOptions = {
    outfile,
    bundle: true,
    entryPoints: ['src/index.ts'],
    define: {
        GIT_HASH: JSON.stringify(git('rev-parse HEAD')),
        GIT_STATUS: JSON.stringify(git('status --short --porcelain')),
    },
    platform: 'node',
    sourcemap: true,
    minify: true,
    color: true,
}

esbuild.build(config)
