import esbuild from 'esbuild'

// ===================
// === Arg Parsing ===
// ===================

import { parseArgs } from 'node:util'
import child_process from 'node:child_process'

let out = parseArgs({
    options: {
        outdir: {
            type: 'string',
        },
    },
})

let outdir = out.values.outdir
if (!outdir) {
    console.error('No --outdir option provided.')
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

const appConfig: esbuild.BuildOptions = {
    outfile: `${outdir}/app.js`,
    bundle: true,
    entryPoints: ['src/runner/index.ts'],
    define: {
        GIT_HASH: JSON.stringify(git('rev-parse HEAD')),
        GIT_STATUS: JSON.stringify(git('status --short --porcelain')),
    },
    platform: 'node',
    sourcemap: true,
    color: true,
}

const shaderExtractorConfig: esbuild.BuildOptions = {
    outfile: `${outdir}/shader-extractor.js`,
    bundle: true,
    entryPoints: ['src/shader-extractor/index.ts'],
    define: {
        GIT_HASH: JSON.stringify(git('rev-parse HEAD')),
        GIT_STATUS: JSON.stringify(git('status --short --porcelain')),
    },
    platform: 'node',
    sourcemap: true,
    color: true,
}

esbuild.build(appConfig)
esbuild.build(shaderExtractorConfig)
