let fss   = require('fs')
let fs    = require('fs').promises
let cmd   = require('./lib/cmd')
let ncp   = require('ncp').ncp
let yargs = require('yargs')



// ====================
// === Global Setup ===
// ====================

let root = __dirname + '/..'
process.chdir(root)

/// Arguments passed to sub-processes called from this script. This variable is set to a specific
/// value after the command line args get parsed.
let subProcessArgs = undefined

/// Arguments passed to a target binary if any. This variable is set to a specific value after the
// command line args get parsed.
let targetArgs = undefined



// =============
// === Utils ===
// =============

/// Copy files and directories.
async function copy(src,tgt) {
    return new Promise((resolve, reject) => {
        ncp(src,tgt,(err) => {
            if (err) { reject(`${err}`) }
            resolve()
        })
    })
}

/// Run the command with the provided args and all args passed to this script after the `--` symbol.
async function run(command,args) {
    await cmd.run(command,args.concat(subProcessArgs))
}


/// Defines a new command argument builder.
function command(docs) {
    return {docs}
}



// ================
// === Commands ===
// ================

let commands = {}


// === Clean ===

commands.clean = command(`Clean all build artifacts`)
commands.clean.js = async function() {
    await cmd.with_cwd('app', async () => {
        await run('npm',['run','clean'])
    })
    try { await fs.unlink('.initialized') } catch {}
}

commands.clean.rust = async function() {
    try { await fs.rmdir('app/generated') } catch {}
    await run('cargo',['clean'])
}


// === Check ===

commands.check = command(`Fast check if project builds (only Rust target)`)
commands.check.rust = async function() {
    await run('cargo',['check'])
}


// === Build ===

commands.build = command(`Build the sources in release mode`)
commands.build.js = async function() {
    console.log(`Building JS target.`)
    await cmd.with_cwd('app', async () => {
        await run('npm',['run','build'])
    })
}

commands.build.rust = async function() {
    console.log(`Building WASM target.`)
    await run('wasm-pack',['build','--target','web','--no-typescript','--out-dir','../../target/web','lib/gui'])
    await patch_file('target/web/gui.js', js_workaround_patcher)
    await fs.rename('target/web/gui_bg.wasm','target/web/gui.wasm')

    /// We build to provisional location and patch files there before copying, so the backpack don't
    /// get errors from processing unpatched files. Also, here we copy into (overwriting), without
    /// removing old files. Backpack on Windows does not tolerate removing files it watches.
    await fs.mkdir('app/generated', {recursive:true})
    await copy('target/web','app/generated/wasm')
}

/// Workaround fix by wdanilo, see: https://github.com/rustwasm/wasm-pack/issues/790
function js_workaround_patcher(code) {
    code = code.replace(/if \(\(typeof URL.*}\);/gs,'return imports')
    code = code.replace(/if \(typeof module.*let result/gs,'let result')
    code = code.replace(/export default init;/gs,'export default init')
    code += '\nexport function after_load\(w,m\) { wasm = w; init.__wbindgen_wasm_module = m;}'
    return code
}

async function patch_file(path,patcher) {
    console.log(`Patching ${path}`)
    let code_to_patch = await fs.readFile(path,'utf8')
    let patched_code  = patcher(code_to_patch)
    await fs.writeFile(path,patched_code)
}


// === Start ===

commands.start = command(`Build and start desktop client`)
commands.start.rust = async function() {
   await commands.build.rust()
}

commands.start.js = async function() {
    console.log(`Building JS target.`)
    await cmd.with_cwd('app', async () => {
        await run('npm',['run','start','--'].concat(targetArgs))
    })
}


// === Test ===

commands.test = command(`Run test suites`)
commands.test.rust = async function() {
    console.log(`Running WASM test suite.`)
    await run('cargo',['test'])

    console.log(`Running WASM visual test suite.`)
    await run('cargo',['run','--manifest-path=build/rust/Cargo.toml','--bin','test-all',
                       '--','--headless','--chrome'])
}


// === Lint ===

commands.lint = command(`Lint the codebase`)
commands.lint.rust = async function() {
    await run('cargo',['clippy','--','-D','warnings'])
}


// === Watch ===

commands.watch = command(`Start a file-watch utility and run interactive mode`)
commands.watch.parallel = true
commands.watch.rust = async function() {
    let target = '"' + 'node ./run build --no-js -- --dev ' + subProcessArgs.join(" ") + '"'
    let args = ['watch','--watch','lib','-s',`${target}`]
    await cmd.run('cargo',args)
}

commands.watch.js = async function() {
    await cmd.with_cwd('app', async () => {
        await run('npm',['run','watch'])
    })
}


// === Dist ===

commands.dist = command(`Build the sources and create distribution packages`)
commands.dist.rust = async function() {
    await commands.build.rust()
}

commands.dist.js = async function() {
    await cmd.with_cwd('app', async () => {
        await run('npm',['run','dist'])
    })
}



// ===========================
// === Command Line Parser ===
// ===========================

let usage = `run command [options]

All arguments after '--' will be passed to build sub-commands.
All arguments after second '--' will be passed to target executable if any.
For example, 'run start -- --dev -- --debug-scene shapes' will pass '--dev' to build \
utilities and '--debug-scene shapes' to the output binary.`

let optParser = yargs
    .scriptName("")
    .usage(usage)
    .help()
    .parserConfiguration({'populate--':true})
    .demandCommand()

optParser.options('rust', {
    describe : 'Run the Rust target',
    type     : 'bool',
    default  : true
})

optParser.options('js', {
    describe : 'Run the JavaScript target',
    type     : 'bool',
    default  : true
})

let commandList = Object.keys(commands)
commandList.sort()
for (let command of commandList) {
    let config = commands[command]
    optParser.command(command,config.docs,(args) => {}, function (argv) {
        subProcessArgs = argv['--']
        if(subProcessArgs === undefined) { subProcessArgs = [] }
        let index = subProcessArgs.indexOf('--')
        if (index == -1) {
            targetArgs = []
        }
        else {
            targetArgs     = subProcessArgs.slice(index + 1)
            subProcessArgs = subProcessArgs.slice(0,index)
        }
        let runner = async function () {
            let do_rust = argv.rust && config.rust
            let do_js   = argv.js   && config.js
            if(config.parallel) {
                let promises = []
                if (do_rust) { promises.push(config.rust(argv)) }
                if (do_js)   { promises.push(config.js(argv)) }
                await Promise.all(promises)
            } else {
                if (do_rust) { await config.rust(argv) }
                if (do_js)   { await config.js(argv)   }
            }
        }
        cmd.section(command)
        runner()
    })
}



// ============
// === Main ===
// ============

async function updateBuildVersion () {
    let config        = {}
    let generatedPath = root + '/app/generated'
    let configPath    = generatedPath + '/build.json'
    let exists        = fss.existsSync(configPath)
    if(exists) {
        let configFile = await fs.readFile(configPath)
        config         = JSON.parse(configFile)
    }
    let commitHashCmd = await cmd.run_read('git',['rev-parse','--short','HEAD'])
    let commitHash    = commitHashCmd.trim()
    if (config.buildVersion != commitHash) {
        config.buildVersion = commitHash
        await fs.mkdir(generatedPath,{recursive:true})
        await fs.writeFile(configPath,JSON.stringify(config,undefined,2))
    }
}

async function main () {
    updateBuildVersion()
    optParser.argv
}

main()
