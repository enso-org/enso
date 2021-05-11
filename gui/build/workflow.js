/// GitHub workflow generator. We are using custom generator in order to re-use sub-steps. This
/// may not be needed anymore after this feature request will be resolved by GitHub:
/// https://github.community/t/support-for-yaml-anchors/16128

const fss   = require('fs')
const path  = require('path')
const paths = require('./paths')
const yaml  = require('js-yaml')



// =================
// === Constants ===
// =================

const NODE_VERSION             = '14.15.0'
const RUST_VERSION             = 'nightly-2019-11-04'
const WASM_PACK_VERSION        = '0.9.1'
const FLAG_NO_CHANGELOG_NEEDED = '[ci no changelog needed]'
const FLAG_FORCE_CI_BUILD      = '[ci build]'



// =============
// === Utils ===
// =============

function job(platforms,name,steps,cfg) {
    if (!cfg) { cfg = {} }
    return {
        name: name,
        "runs-on": "${{ matrix.os }}",
        strategy: {
            matrix: {
              os: platforms
            },
            "fail-fast": false
        },
        // WARNING!
        // Do not update to `checkout@v2` because it is broken:
        // https://github.com/actions/checkout/issues/438
        steps : list({uses:"actions/checkout@v1"}, ...steps),
        ...cfg
    }
}

function job_on_all_platforms(...args) {
    return job(["windows-latest", "macOS-latest", "ubuntu-latest"],...args)
}

function job_on_macos(...args) {
    return job(["macOS-latest"],...args)
}

function job_on_ubuntu(...args) {
    return job(["ubuntu-latest"],...args)
}

function job_on_ubuntu_18_04(...args) {
    return job(["ubuntu-18.04"],...args)
}

function list(...args) {
    let out = []
    for (let arg of args) {
        if (Array.isArray(arg)) {
            out.push(...arg)
        } else {
            out.push(arg)
        }
    }
    return out
}



// ============
// === Info ===
// ============

dumpGitHubContext = {
    name: 'Dump GitHub context',
    env: {
        GITHUB_CONTEXT: '${{ toJson(github) }}'
    },
    run: 'echo "$GITHUB_CONTEXT"'
}



// ====================
// === Dependencies ===
// ====================

let installRust = {
    name: "Install Rust",
    uses: "actions-rs/toolchain@v1",
    with: {
        toolchain: RUST_VERSION,
        override: true
    }
}

let installNode = {
    name: "Install Node",
    uses: "actions/setup-node@v1",
    with: {
        "node-version": NODE_VERSION,
    }
}

let installPrettier = {
    name: "Install Prettier",
    run: "npm install --save-dev --save-exact prettier"
}

let installClippy = {
    name: "Install Clippy",
    run: "rustup component add clippy"
}


function installWasmPackOn(name,sys,pkg) {
    return {
        name: `Install wasm-pack (${name})`,
        env: {
            WASMPACKURL: `https://github.com/rustwasm/wasm-pack/releases/download/v${WASM_PACK_VERSION}`,
            WASMPACKDIR: `wasm-pack-v${WASM_PACK_VERSION}-x86_64-${pkg}`,
        },
        run: `
            curl -L "$WASMPACKURL/$WASMPACKDIR.tar.gz" | tar -xz -C .
            mv $WASMPACKDIR/wasm-pack ~/.cargo/bin
            rm -r $WASMPACKDIR`,
        shell: "bash",
        if: `startsWith(matrix.os,'${sys}')`,
    }
}

let installWasmPackOnMacOS   = installWasmPackOn('macOS','macOS','apple-darwin')
let installWasmPackOnWindows = installWasmPackOn('Windows','windows','pc-windows-msvc')
let installWasmPackOnLinux   = installWasmPackOn('Linux','ubuntu','unknown-linux-musl')

// We could use cargo install wasm-pack, but that takes 3.5 minutes compared to few seconds.
let installWasmPack = [installWasmPackOnMacOS, installWasmPackOnWindows, installWasmPackOnLinux]



// =============================
// === Build, Lint, and Test ===
// =============================

function buildOn(name,sys) {
    return {
        name: `Build (${name})`,
        run: `node ./run dist --skip-version-validation --target ${name}`,
        if: `startsWith(matrix.os,'${sys}')`
    }
}

buildOnMacOS   = buildOn('macos','macos')
buildOnWindows = buildOn('win','windows')
buildOnLinux   = buildOn('linux','ubuntu')

let lintMarkdown = {
    name: "Lint Markdown sources",
    run: "npx prettier --check '*.md'",
}

let lintJavaScript = {
    name: "Lint JavaScript sources",
    run: "npx prettier --check 'src/**/*.js'",
}

let lintRust = {
    name: "Lint Rust sources",
    run: "node ./run lint --skip-version-validation",
}

let testNoWASM = {
    name: "Run tests (no WASM)",
    run: "node ./run test --no-wasm --skip-version-validation",
}

let testWASM = {
    name: "Run tests (WASM)",
    run: "node ./run test --no-native --skip-version-validation",
}



// =================
// === Artifacts ===
// =================

let uploadContentArtifacts = {
    name: `Upload Content Artifacts`,
    uses: "actions/upload-artifact@v1",
    with: {
       name: 'content',
       path: `dist/content`
    },
    if: `startsWith(matrix.os,'macOS')`
}

function uploadBinArtifactsFor(name,sys,ext,os) {
    return {
        name: `Upload Artifacts (${name}, ${ext})`,
        uses: "actions/upload-artifact@v1",
        with: {
           name: `enso-${os}-\${{fromJson(steps.changelog.outputs.content).version}}.${ext}`,
           path: `dist/client/enso-${os}-\${{fromJson(steps.changelog.outputs.content).version}}.${ext}`
        },
        if: `startsWith(matrix.os,'${sys}')`
    }
}

function uploadBinArtifactsWithChecksumsFor(name,sys,ext,os) {
    return [
        uploadBinArtifactsFor(name,sys,ext,os),
        uploadBinArtifactsFor(name,sys,ext+'.sha256',os)
    ]
}

uploadBinArtifactsForMacOS   = uploadBinArtifactsWithChecksumsFor('macOS','macos','dmg','mac')
uploadBinArtifactsForLinux   = uploadBinArtifactsWithChecksumsFor('Linux','ubuntu','AppImage','linux')
uploadBinArtifactsForWindows = uploadBinArtifactsWithChecksumsFor('Windows','windows','exe','win')

let downloadArtifacts = {
    name: "Download artifacts",
    uses: "actions/download-artifact@v2",
    with: {
        path: "artifacts"
    }
}



// ===========
// === Git ===
// ===========

/// Gets a space-separated list of changed files between this commit and the `develop` branch.
let getListOfChangedFiles = {
    name: 'Get list of changed files',
    id: 'changed_files',
    run: `
        list=\`git diff --name-only origin/\${{github.base_ref}} HEAD | tr '\\n' ' '\`
        echo $list
        echo "::set-output name=list::'$list'"
    `,
    shell: 'bash',
    if: `github.base_ref == 'develop' || github.base_ref == 'unstable' || github.base_ref == 'stable'`
}



// =================
// === Changelog ===
// =================

let getCurrentReleaseChangelogInfo = {
    name: 'Read changelog info',
    id: 'changelog',
    run: `
        node ./run ci-gen --skip-version-validation
        content=\`cat CURRENT_RELEASE_CHANGELOG.json\`
        echo "::set-output name=content::$content"
    `,
    shell: 'bash'
}

let assertChangelogWasUpdated = [
    getListOfChangedFiles,
    {
        name: 'Assert if CHANGELOG.md was updated (on pull request)',
        run: `if [[ \${{ contains(steps.changed_files.outputs.list,'CHANGELOG.md') || contains(github.event.head_commit.message,'${FLAG_NO_CHANGELOG_NEEDED}') }} == false ]]; then exit 1; fi`,
        if: `github.base_ref == 'develop' || github.base_ref == 'unstable' || github.base_ref == 'stable'`
    }
]



// ======================
// === GitHub Release ===
// ======================

let uploadGitHubRelease = [
    installPrettier,
    {
        name: `Pretty print changelog.`,
        run: "npx prettier --prose-wrap never CHANGELOG.md --write"
    },
    {
        name: `Upload GitHub Release`,
        uses: "softprops/action-gh-release@v1",
        env: {
            GITHUB_TOKEN: "${{ secrets.GITHUB_TOKEN }}"
        },
        with: {
            files:      "artifacts/**/enso-*",
            name:       "Enso ${{fromJson(steps.changelog.outputs.content).version}}",
            tag_name:   "v${{fromJson(steps.changelog.outputs.content).version}}",
            body:       "${{fromJson(steps.changelog.outputs.content).body}}",
            prerelease: "${{fromJson(steps.changelog.outputs.content).prerelease}}",
        },
    }
]



// ===================
// === CDN Release ===
// ===================

prepareAwsSessionCDN = {
    shell: "bash",
    run: `
        aws configure --profile s3-upload <<-EOF > /dev/null 2>&1
        \${{ secrets.ARTEFACT_S3_ACCESS_KEY_ID }}
        \${{ secrets.ARTEFACT_S3_SECRET_ACCESS_KEY }}
        us-west-1
        text
        EOF
    `
}

function uploadToCDN(...names) {
    const actions = []
    for (let name of names) {
        const action = {
            name: `Upload '${name}' to CDN`,
            shell: "bash",
            run: `aws s3 cp ./artifacts/content/assets/${name} `
               + `s3://ensocdn/ide/\${{fromJson(steps.changelog.outputs.content).version}}/${name} --profile `
               + `s3-upload --acl public-read`
        }
        if (name.endsWith(".gz")) {
            action.run += " --content-encoding gzip";
        }
        if (name.endsWith(".wasm")) {
            action.run += " --content-type 'application/wasm'";
        }
        actions.push(action)
    }
    return actions
}



// ==================
// === Assertions ===
// ==================

let assertVersionUnstable = {
    name: "Assert Version Unstable",
    run: "node ./run assert-version-unstable --skip-version-validation",
    if: `github.ref == 'refs/heads/unstable' || github.base_ref == 'unstable'`
}

let assertVersionStable = {
    name: "Assert Version Stable",
    run: "node ./run assert-version-stable --skip-version-validation",
    if: `github.ref == 'refs/heads/stable' || github.base_ref == 'stable'`
}

let assertReleaseDoNotExists = [
    {
        id: 'checkCurrentReleaseTag',
        uses: 'mukunku/tag-exists-action@v1.0.0',
        env: {
            GITHUB_TOKEN: "${{ secrets.GITHUB_TOKEN }}"
        },
        with: {
            tag: 'v${{fromJson(steps.changelog.outputs.content).version}}'
        }
    },
    {
        name: 'Fail if release already exists',
        run: 'if [[ ${{ steps.checkCurrentReleaseTag.outputs.exists }} == true ]]; then exit 1; fi',
        if: `github.base_ref == 'unstable' || github.base_ref == 'stable'`
    }
]

assertNoSquashCommitForRelease = {
    name: `Fail if squash commit to the 'unstable' or the 'stable' branch.`,
    run: 'if [[ "${{ github.base_ref }}" == "unstable" || "${{ github.base_ref }}" == "stable" ]]; then exit 1; fi',
}

let assertions = list(
    assertVersionUnstable,
    assertVersionStable,
    assertReleaseDoNotExists,
    assertChangelogWasUpdated,
    assertNoSquashCommitForRelease,
)



// ===============
// === Workflow ===
// ===============

/// Make a release only if it was a push to 'unstable' or 'stable'. Even if it was a pull request
/// FROM these branches, the `github.ref` will be different.
let releaseCondition = `github.ref == 'refs/heads/unstable' || github.ref == 'refs/heads/stable'`

/// Make a full build if one of the following conditions is true:
/// 1. There was a `FLAG_FORCE_CI_BUILD` flag set in the commit message (see its docs for more info).
/// 2. It was a pull request to the 'unstable', or the 'stable' branch.
/// 3. It was a commit to the 'develop' branch.
/// Otherwise, perform a simplified (faster) build only.
let buildCondition = `contains(github.event.pull_request.body,'${FLAG_FORCE_CI_BUILD}') || contains(github.event.head_commit.message,'${FLAG_FORCE_CI_BUILD}') || github.ref == 'refs/heads/develop' || github.base_ref == 'unstable' || github.base_ref == 'stable' || (${releaseCondition})`

let workflow = {
    name : "GUI CI",
    on: {
        push: {
            branches: ['develop','unstable','stable']
        },
        pull_request: {}
    },
    jobs: {
        info: job_on_macos("Build Info", [
            dumpGitHubContext
        ]),
        version_assertions: job_on_macos("Assertions", [
            getCurrentReleaseChangelogInfo,
            assertions
        ]),
        lint: job_on_macos("Linter", [
            installNode,
            installRust,
            installPrettier,
            installClippy,
            lintMarkdown,
            lintJavaScript,
            lintRust
        ]),
        test: job_on_macos("Tests", [
            installNode,
            installRust,
            testNoWASM,
        ]),
        "wasm-test": job_on_macos("WASM Tests", [
            installNode,
            installRust,
            installWasmPack,
            testWASM
        ]),
        simple_build: job_on_macos("Simple Build (WASM size limit check)", [
            installNode,
            installRust,
            installWasmPack,
            buildOnMacOS,
        ],{if:`!(${buildCondition})`}),
        build: job_on_all_platforms("Build", [
            getCurrentReleaseChangelogInfo,
            installNode,
            installRust,
            installWasmPack,
            buildOnMacOS,
            buildOnWindows,
            buildOnLinux,
            uploadContentArtifacts,
            uploadBinArtifactsForMacOS,
            uploadBinArtifactsForWindows,
            uploadBinArtifactsForLinux,
        ],{if:buildCondition}),
        release_to_github: job_on_macos("GitHub Release", [
            downloadArtifacts,
            getCurrentReleaseChangelogInfo,
            // This assertion is checked earlier, but we should double-check it in case several
            // CI jobs wil be run on this branch and a release was created when this workflow was
            // running.
            assertReleaseDoNotExists,
            uploadGitHubRelease,
        ],{ if:releaseCondition,
            needs:['version_assertions','lint','test','wasm-test','build']
        }),
        release_to_cdn: job_on_ubuntu_18_04("CDN Release", [
            downloadArtifacts,
            getCurrentReleaseChangelogInfo,
            prepareAwsSessionCDN,
            uploadToCDN('index.js.gz','style.css','ide.wasm','wasm_imports.js.gz'),
        ],{ if:releaseCondition,
            needs:['version_assertions','lint','test','wasm-test','build']
        })
    }
}

let header = `
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# DO NOT CHANGE THIS FILE. IT WAS GENERATED FROM 'build/workflow.js'. READ DOCS THERE TO LEARN MORE.
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
`

/// Generates a new GitHub workflow file (in .github/workflow/...).
function generate() {
    let workflow_script = header + '\n' + yaml.dump(workflow,{noRefs:true})
    fss.writeFileSync(path.join(paths.github.workflows,'gui-ci.yml'),workflow_script)
}



// ===============
// === Exports ===
// ===============

module.exports = {generate}
