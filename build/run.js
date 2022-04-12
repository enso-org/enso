const child_process = require('child_process')
const cmd = require('./cmd')
const fs = require('fs').promises
const fss = require('fs')
const unzipper = require('unzipper')
const glob = require('glob')
const os = require('os')
const path = require('path')
const paths = require('./paths')
const prettier = require('prettier')
const release = require('./release')
const stream = require('stream')
const workflow = require('./workflow')
const yargs = require('yargs')
const zlib = require('zlib')
const { promisify } = require('util')
const pipe = promisify(stream.pipeline)

// ==============
// === Errors ===
// ==============

process.on('unhandledRejection', error => {
    throw error
})
process.chdir(paths.root)

// ========================
// === Global Variables ===
// ========================

/// Arguments passed to cargo build system called from this script. This variable is set to a
/// specific value after the command line args get parsed.
let cargoArgs = undefined

/// Arguments passed to a target binary if any. This variable is set to a specific value after the
// command line args get parsed.
let targetArgs = undefined

// =============
// === Utils ===
// =============

async function gzip(input, output) {
    const gzip = zlib.createGzip()
    const source = fss.createReadStream(input)
    await fs.mkdir(path.dirname(output), { recursive: true })
    const destination = fss.createWriteStream(output)
    await pipe(source, gzip, destination)
}

/// Run the command with the provided args and all args passed to this script after the `--` symbol.
async function run_cargo(command, args) {
    await cmd.run(command, args.concat(cargoArgs))
}

/// Run the command with the provided args and all args passed to this script after the `--` symbol.
async function run(command, args) {
    await cmd.run(command, args)
}

/// Defines a new command argument builder.
function command(docs) {
    return { docs }
}

/// Build the project manager module, which downloads the project manager binary for the current
/// platform.
async function build_project_manager() {
    console.log(`Getting project manager manager.`)
    await cmd.with_cwd(paths.ide_desktop.lib.projectManager, async () => {
        await run('npm', ['run-script build'])
    })
}

/// Run the local project manager binary.
function run_project_manager(options = {}) {
    const bin_path = paths.get_project_manager_path(paths.dist.bin)
    console.log(`Starting the project manager from "${bin_path}".`)
    return child_process.execFile(bin_path, [], options, (error, stdout, stderr) => {
        console.log(`Project manager finished.`)
        console.error(stderr)
        if (error && !error.killed) {
            throw error
        }
        console.log(stdout)
    })
}

// ================
// === Commands ===
// ================

const DEFAULT_CRATE = 'app/gui'
let commands = {}

// === Clean ===

commands.clean = command(`Clean all build artifacts`)
commands.clean.js = async function () {
    await cmd.with_cwd(paths.ide_desktop.root, async () => {
        await run('npm', ['run', 'clean'])
    })
    try {
        await fs.unlink(paths.dist.init)
        await fs.unlink(paths.dist.buildInit)
    } catch {}
}

commands.clean.rust = async function () {
    await run_cargo('cargo', ['clean'])
}

// === Check ===

commands.check = command(`Fast check if project builds (only Rust target)`)
commands.check.rust = async function () {
    await run_cargo('cargo', [
        'check',
        '--workspace',
        ' -p',
        'enso-integration-test',
        '--all-targets',
    ])
}

// === Build ===

commands.build = command(`Build the sources in release mode`)
commands.build.options = {
    crate: {
        describe: 'Target crate to build',
        type: 'string',
    },
}
commands.build.js = async function () {
    await installJsDeps()
    console.log(`Building JS target.`)
    await run('npm', ['run', 'build'])
}

// We build WASM binaries from Rust code using `wasm-pack`. Intermediate temporary directory is used
// before final copy to the Webpack's `dist` location because of two reasons:
// 1. Webpack triggers recompilation on file changes, and we don't want to bother it until the final
// binaries are ready to use.
// 2. `wasm-pack` clears its output directory before compilation, which breaks Webpack because of
// missing files.
commands.build.rust = async function (argv) {
    let crate = argv.crate || DEFAULT_CRATE
    let crate_sfx = crate ? ` '${crate}'` : ``
    console.log(`Building WASM target${crate_sfx}.`)
    let args = [
        'build',
        '--target',
        'web',
        '--out-dir',
        paths.wasm.root,
        '--out-name',
        'ide',
        crate,
    ]

    if (argv.dev) {
        args.push('--dev')
    }
    // Use the environment-variable API provided by the `enso_profiler_macros` library to implement
    // the public interface to profiling-level configuration
    // (see: https://github.com/enso-org/design/blob/main/epics/profiling/implementation.md)
    if (argv['profiling-level']) {
        process.env.ENSO_MAX_PROFILING_LEVEL = argv['profiling-level']
    }
    args.push('--')
    // Enable a Rust unstable feature that the `#[profile]` macro uses to obtain source-file and line
    // number information to include in generated profile files.
    //
    // The IntelliJ Rust plugin does not support the `proc_macro_span` Rust feature; using it causes
    // JetBrains IDEs to become entirely unaware of the items produced by `#[profile]`.
    // (See: https://github.com/intellij-rust/intellij-rust/issues/8655)
    //
    // In order to have line number information in actual usage, but keep everything understandable
    // by JetBrains IDEs, we need IntelliJ/CLion to build crates differently from how they are
    // built for the application to be run. This is accomplished by gating the use of the unstable
    // functionality by a `cfg` flag. A `cfg` flag is disabled by default, so when a Rust IDE builds
    // crates internally in order to determine macro expansions, it will do so without line numbers.
    // When this script is used to build the application, it is not for the purpose of IDE macro
    // expansion, so we can safely enable line numbers.
    //
    // The reason we don't use a Cargo feature for this is because this script can build different
    // crates, and we'd like to enable this feature when building any crate that depends on the
    // `profiler` crates. We cannot do something like '--feature=enso_profiler/line-numbers' without
    // causing build to fail when building a crate that doesn't have `enso_profiler` in its
    // dependency tree.
    process.env.ENSO_ENABLE_PROC_MACRO_SPAN = 1
    await run_cargo('wasm-pack', args)
    await patch_file(paths.wasm.glue, js_workaround_patcher)
    await fs.rename(paths.wasm.mainRaw, paths.wasm.main)
    if (!argv.dev) {
        console.log('Minimizing the WASM binary.')
        await gzip(paths.wasm.main, paths.wasm.mainGz)

        const limitMb = 4.65
        await checkWasmSize(paths.wasm.mainGz, limitMb)
    }
    // Copy WASM files from temporary directory to Webpack's `dist` directory.
    await fs.cp(paths.wasm.root, paths.dist.wasm.root, { recursive: true })
}

// Check if compressed WASM binary exceeds the size limit.
async function checkWasmSize(path, limitMb) {
    console.log('Checking the resulting WASM size.')
    let stats = fss.statSync(path)
    let size = Math.round((100 * stats.size) / 1024 / 1024) / 100
    if (size > limitMb) {
        throw `Output file size exceeds the limit (${size}MB > ${limitMb}MB).`
    }
}

/// Workaround fix by wdanilo, see: https://github.com/rustwasm/wasm-pack/issues/790
function js_workaround_patcher(code) {
    code = code.replace(/if \(typeof input === 'string'.*return wasm;/gs, 'return imports')
    code = code.replace(
        /if \(typeof input === 'undefined'.*const imports = {};/gs,
        'const imports = {};'
    )
    code = code.replace(/export default init;/gs, 'export default init')
    code += '\nexport function after_load(w,m) { wasm = w; init.__wbindgen_wasm_module = m;}'
    return code
}

async function patch_file(path, patcher) {
    let code_to_patch = await fs.readFile(path, 'utf8')
    let patched_code = patcher(code_to_patch)
    await fs.writeFile(path, patched_code)
}

// === Start ===

commands.start = command(`Build and start desktop client`)
commands.start.rust = async function (argv) {
    let argv2 = Object.assign({}, argv, { dev: true })
    await commands.build.rust(argv2)
}

commands.start.js = async function (argv) {
    await installJsDeps()
    console.log(`Building JS target.` + argv)
    // The backend path is being prepended here, as appending would be incorrect.
    // That is because `targetArgs` might include `-- …` and appended args could
    // end up being passed to the spawned backend process.
    const args = ['--backend-path', paths.get_project_manager_path(paths.dist.bin)].concat(
        targetArgs
    )
    if (argv.dev) {
        args.push('--dev')
    }
    await cmd.with_cwd(paths.ide_desktop.root, async () => {
        await run('npm', ['run', 'start', '--'].concat(args))
    })
}

// === Test ===

commands.test = command(`Run test suites`)
commands.test.rust = async function (argv) {
    if (argv.native) {
        console.log(`Running Rust test suite.`)
        await run_cargo('cargo', ['test', '--workspace'])
    }

    if (argv.wasm) {
        console.log(`Running Rust WASM test suite.`)
        let args = [
            'run',
            '--manifest-path=build/rust-scripts/Cargo.toml',
            '--bin',
            'test_all',
            '--',
            '--headless',
            '--chrome',
        ]
        process.env.WASM_BINDGEN_TEST_TIMEOUT = 60
        await run_cargo('cargo', args)
    }
}

// === Integration Test ===

commands['integration-test'] = command('Run integration test suite')
commands['integration-test'].rust = async function (argv) {
    let pm_process = null
    if (argv.backend !== 'false') {
        let env = { ...process.env, PROJECTS_ROOT: path.resolve(os.tmpdir(), 'enso') }
        pm_process = await build_project_manager().then(() => run_project_manager({ env: env }))
    }
    try {
        console.log(`Running Rust WASM test suite.`)
        process.env.WASM_BINDGEN_TEST_TIMEOUT = 300
        let args = [
            'test',
            '--headless',
            '--chrome',
            'integration-test',
            '--profile=integration-test',
        ]
        await run_cargo('wasm-pack', args)
    } finally {
        console.log(`Shutting down Project Manager`)
        if (pm_process !== null) {
            pm_process.kill()
        }
    }
}

// === Lint ===

commands.lint = command(`Lint the codebase`)
commands.lint.rust = async function () {
    await run_cargo('cargo', [
        'clippy',
        '--workspace',
        '-p',
        'enso-integration-test',
        '--all-targets',
        '--',
        '-D',
        'warnings',
    ])
    await run_cargo('cargo', ['fmt', '--', '--check'])
}

// === TomlFmt ===

commands['toml-fmt'] = command(`Lint the codebase`)
commands['toml-fmt'].rust = async function () {
    console.log('Looking for all TOML files.')
    let files = glob.sync(paths.root + '/**/*.toml', { cwd: paths.root })
    console.log(`Found ${files.length} entries. Running auto-formatter.`)
    for (let file of files) {
        console.log(`    Formatting '${file}'.`)
        let text = fss.readFileSync(file, 'utf8')
        let out = prettier.format(text, { parser: 'toml' })
        fss.writeFileSync(file, out)
    }
}

// === Watch ===

commands.watch = command(`Start a file-watch utility and run interactive mode`)
commands.watch.options = Object.assign({}, commands.build.options)
commands.watch.parallel = false
commands.watch.common = async function (argv) {
    argv.dev = true

    // Init JS build and project manager.

    await installJsDeps()
    if (argv.backend !== 'false') {
        await build_project_manager().then(run_project_manager)
    }

    // Run build processes.

    await cmd.with_cwd(paths.root, async () => {
        return commands.build.rust(argv)
    })
    await cmd.with_cwd(paths.ide_desktop.root, async () => {
        // Among other things, this will call the build script of the project-manager package. But
        // this is unnecessary because that script is already called by `build_project_manager`
        // above.
        return commands.build.js(argv)
    })

    // Run watch processes.

    const rust_process = cmd.with_cwd(paths.root, async () => {
        let build_args = []
        if (argv.crate !== undefined) {
            build_args.push(`--crate=${argv.crate}`)
        }
        build_args = build_args.join(' ')
        const shellCommand =
            '"' +
            `node ${paths.script.main} build --skip-version-validation --no-js --dev ${build_args} -- ` +
            cargoArgs.join(' ') +
            '"'
        // We ignore changes in README.md because `wasm-pack` copies it, which triggers `cargo watch`
        // because of this bug: https://github.com/notify-rs/notify/issues/259
        const ignore = ['--ignore', 'README.md']
        let args = new Array().concat('watch', ignore, '-s', `${shellCommand}`)
        return cmd.run('cargo', args)
    })
    const js_process = cmd.with_cwd(paths.ide_desktop.root, async () => {
        return run('npm', ['run', 'watch'])
    })

    await rust_process
    await js_process
}

// === Dist ===

commands.dist = command(`Build the sources and create distribution packages`)
commands.dist.rust = async function (argv) {
    await commands.build.rust(argv)
}

commands.dist.js = async function () {
    await installJsDeps()
    await cmd.with_cwd(paths.ide_desktop.root, async () => {
        await run('npm', ['run', 'dist'])
    })
}

// === CI Gen ===

/// The command is used by CI to generate the file `CURRENT_RELEASE_CHANGELOG.json`, which contains
/// information about the newest release. It is then used by CI to generate version and description
/// of the product release.
commands['ci-gen'] = command(`Generate CI build related files`)
commands['ci-gen'].rust = async function (argv) {
    let entry = release.changelog().newestEntry()
    let body = entry.body
    let version = entry.version.toString()
    let prerelease = entry.isPrerelease()
    let obj = { version, body, prerelease }
    let json = JSON.stringify(obj)
    fss.writeFileSync(path.join(paths.root, 'CURRENT_RELEASE_CHANGELOG.json'), json)
}

/// Asserts whether the current version of the package (newest in CHANGELOG.md) is unstable.
commands['assert-version-unstable'] = command(`Assert the current version is unstable`)
commands['assert-version-unstable'].rust = async function (argv) {
    let entry = release.changelog().newestEntry().assert_is_unstable()
}

/// Asserts whether the current version of the package (newest in CHANGELOG.md) is stable.
commands['assert-version-stable'] = command(`Assert the current version is stable`)
commands['assert-version-stable'].rust = async function (argv) {
    let entry = release.changelog().newestEntry().assert_is_stable()
}

// ===========================
// === Command Line Parser ===
// ===========================

let usage = `run command [options]

All arguments after '--' will be passed to cargo build system.
All arguments after second '--' will be passed to target executable if any.
For example, 'run start -- --dev -- --debug-scene shapes' will pass '--dev' to cargo \
and '--debug-scene shapes' to the output binary.`

let optParser = yargs
    .scriptName('')
    .usage(usage)
    .help()
    .parserConfiguration({ 'populate--': true })
    .demandCommand()

optParser.options('rust', {
    describe: 'Run the Rust target',
    type: 'bool',
    default: true,
})

optParser.options('js', {
    describe: 'Run the JavaScript target',
    type: 'bool',
    default: true,
})

optParser.options('release', {
    describe: 'Enable all optimizations',
    type: 'bool',
})

optParser.options('dev', {
    describe: 'Optimize for fast builds',
    type: 'bool',
})

optParser.options('target', {
    describe:
        'Set the build target. Defaults to the current platform. ' +
        'Valid values are: "linux" "macos" and "win"',
    type: 'string',
})

optParser.options('backend', {
    describe: 'Start the backend process automatically [true]',
    type: 'bool',
    default: true,
})

let commandList = Object.keys(commands)
commandList.sort()
for (let command of commandList) {
    let config = commands[command]
    optParser.command(command, config.docs, args => {
        for (let option in config.options) {
            args.options(option, config.options[option])
        }
        for (let arg in config.args) {
            args.positional(arg, config.args[arg])
        }
        args.options('native', {
            describe: 'Run native tests',
            type: 'bool',
            default: true,
        })
        args.options('wasm', {
            describe: 'Run WASM tests',
            type: 'bool',
            default: true,
        })
    })
}

// ======================
// === Package Config ===
// ======================

function defaultConfig() {
    return {
        version: `${release.currentVersion()}`,
        author: {
            name: 'Enso Team',
            email: 'contact@enso.org',
        },
        homepage: 'https://github.com/enso-org/ide',
        repository: {
            type: 'git',
            url: 'git@github.com:enso-org/ide.git',
        },
        bugs: {
            url: 'https://github.com/enso-org/ide/issues',
        },
    }
}

async function processPackageConfigs() {
    let files = []
    files = files.concat(glob.sync(paths.ide_desktop.root + '/package.js', { cwd: paths.root }))
    files = files.concat(
        glob.sync(paths.ide_desktop.root + '/lib/*/package.js', { cwd: paths.root })
    )
    for (file of files) {
        let dirPath = path.dirname(file)
        let outPath = path.join(dirPath, 'package.json')
        let src = await fs.readFile(file, 'utf8')
        let modSrc = `module = {}\n${src}\nreturn module.exports`
        let fn = new Function('require', 'paths', modSrc)
        let mod = fn(require, paths)
        let config = mod.config
        if (!config) {
            throw `Package config '${file}' do not export 'module.config'.`
        }
        config = Object.assign(defaultConfig(), config)
        fs.writeFile(outPath, JSON.stringify(config, undefined, 4))
    }
}

// ============
// === Main ===
// ============

async function updateBuildVersion(argv) {
    const target = get_target_platform(argv)
    let config = {}
    let configPath = paths.dist.buildInfo
    let exists = fss.existsSync(configPath)
    if (exists) {
        let configFile = await fs.readFile(configPath)
        config = JSON.parse(configFile)
    }

    let commitHashCmd = await cmd.run_read('git', ['rev-parse', '--short', 'HEAD'])
    let commitHash = commitHashCmd.trim()

    if (config.buildVersion !== commitHash || config.target !== target) {
        config.target = target
        config.buildVersion = commitHash
        await fs.mkdir(paths.dist.root, { recursive: true })
        await fs.writeFile(configPath, JSON.stringify(config, undefined, 2))
    }
}

async function installJsDeps() {
    let initialized = isInitialized()
    let isCI = process.env.hasOwnProperty('CI')
    // We update JS dependencies on CI every time to ensure that incremental build is always possible.
    // Otherwise adding new dependency will require a clean build on CI.
    if (!initialized || isCI) {
        console.log('Installing application dependencies.')
        await cmd.with_cwd(paths.ide_desktop.root, async () => {
            await cmd.run('npm', ['run', 'install'])
        })
    }
    if (!initialized) {
        console.log('Downloading binary assets.')
        await downloadJsAssets()
        markAsInitialized()
    }
}

// Return true if markAsInitialized was run in the past, otherwise return false.
function isInitialized() {
    return fss.existsSync(paths.dist.init)
}

// Create an empty file at paths.dist.init to signal that the initialization was done.
async function markAsInitialized() {
    await fs.mkdir(paths.dist.root, { recursive: true })
    let handle = await fs.open(paths.dist.init, 'w')
    await handle.close()
}

async function downloadJsAssets() {
    const workdir = path.join(paths.root, '.assets-temp')
    await fs.mkdir(workdir, { recursive: true })
    const ideAssetsMainZip = 'ide-assets-main.zip'
    const ideAssetsUrl = `https://github.com/enso-org/ide-assets/archive/refs/heads/main.zip`
    const unzippedAssets = path.join(workdir, 'ide-assets-main', 'content', 'assets')
    const jsLibAssets = path.join(paths.ide_desktop.lib.content, 'assets')
    await cmd.with_cwd(workdir, async () => {
        await cmd.run('curl', [
            '--retry',
            '4',
            '--retry-connrefused',
            '-fsSL',
            '-o',
            ideAssetsMainZip,
            ideAssetsUrl,
        ])
    })

    const assetsArchive = await unzipper.Open.file(path.join(workdir, ideAssetsMainZip))
    await assetsArchive.extract({ path: workdir })
    await fs.cp(unzippedAssets, jsLibAssets, { recursive: true })
    await fs.rm(workdir, { recursive: true, force: true })
}

async function runCommand(command, argv) {
    let config = commands[command]
    cargoArgs = argv['--']
    if (config === undefined) {
        console.error(`Invalid command '${command}'.`)
        return
    }
    if (cargoArgs === undefined) {
        cargoArgs = []
    }
    let index = cargoArgs.indexOf('--')
    if (index == -1) {
        targetArgs = []
    } else {
        targetArgs = cargoArgs.slice(index + 1)
        cargoArgs = cargoArgs.slice(0, index)
    }
    let runner = async function () {
        let do_common = config.common
        let do_rust = argv.rust && config.rust
        let do_js = argv.js && config.js

        let commonCmd = () => cmd.with_cwd(paths.root, async () => await config.common(argv))
        let rustCmd = () => cmd.with_cwd(paths.root, async () => await config.rust(argv))
        let jsCmd = () => cmd.with_cwd(paths.ide_desktop.root, async () => await config.js(argv))
        if (config.parallel) {
            let promises = []
            if (do_common) {
                promises.push(commonCmd())
            }
            if (do_rust) {
                promises.push(rustCmd())
            }
            if (do_js) {
                promises.push(jsCmd())
            }
            await Promise.all(promises)
        } else {
            if (do_common) {
                await commonCmd()
            }
            if (do_rust) {
                await rustCmd()
            }
            if (do_js) {
                await jsCmd()
            }
        }
    }
    cmd.section(command)
    runner()
}

function get_target_platform(argv) {
    let target = argv.target
    if (target === undefined) {
        const local_platform = os.platform()
        switch (local_platform) {
            case 'darwin':
                return 'macos'
            case 'win32':
                return 'win'
            default:
                return local_platform
        }
    }
    return target
}

async function main() {
    let argv = optParser.parse()
    await updateBuildVersion(argv)
    await processPackageConfigs()
    workflow.generate()
    let command = argv._[0]
    await runCommand(command, argv)
}

main()
