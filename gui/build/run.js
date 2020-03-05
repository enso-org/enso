let fss = require('fs')
let fs  = require('fs').promises
let cmd = require('./lib/cmd')
let ncp = require('ncp').ncp

let argv = require('minimist')(process.argv.slice(2),{'--':true})
let child_argv = argv['--']

let root = __dirname + '/..'
process.chdir(root)



// ============
// === Help ===
// ============

const HELP_MESSAGE = `
Enso building utility.

Usage: run command [options]
Please note that all arguments after '--' will be passed to sub-commands.

Commands:
    help     Print this help message.
    clean    Clean all build artifacts.
    check    Fast check if project builds (only Rust target).
    build    Build the sources.
    dist     Build the sources and create distribution packages.
    watch    Start a file-watch utility and run interactive mode.

Options:
    --help   Print this help message
    --js     Run the JavaScript target [true].
    --rust   Run the Rust target [true].
`

function print_help () {
    console.log(HELP_MESSAGE)
    process.exit()
}

function validate_options() {
    let args_check = Object.assign({},argv)
    for (arg of ['_','--','validation','help','rust','js']) {
        delete args_check[arg]
    }

    let unrecognized = Object.keys(args_check)
    if (unrecognized.length > 0) {
        console.error(`[ERROR] The following arguments were not recognized: ${unrecognized}.`)
        console.error(`Use --help to learn about available commands and options.`)
        process.exit()
    }
}
validate_options()



// =============
// === Utils ===
// =============

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
    await cmd.run(command,args.concat(child_argv))
}



// =============
// === Clean ===
// =============

async function clean_js () {
    await cmd.with_cwd('app', async () => {
        await run('npm',['run','clean'])
    })
    try { await fs.unlink('.initialized') } catch {}
}

async function clean_rust () {
    try { await fs.rmdir('app/generated') } catch {}
    await run('cargo',['clean'])
}



// =============
// === Check ===
// =============

async function check_rust() {
    await run('cargo',['check'])
}

async function check_js() {}



// =============
// === Build ===
// =============

async function build_js () {
    console.log(`Building JS target.`)
    await cmd.with_cwd('app', async () => {
        await run('npm',['run','build'])
    })
}

async function build_rust () {
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



// ============
// === Lint ===
// ============

async function lint_rust() {
    await run('cargo',['clippy','--','-D','warnings'])
}

async function lint_js() {}



// =============
// === Watch ===
// =============

async function watch_rust () {
    let target = '"' + 'node ./run build -- --dev ' + child_argv.join(" ") + '"'
    let args = ['watch','--watch','lib','-s',`${target}`]
    await cmd.run('cargo',args)
}

async function watch_js () {
    await cmd.with_cwd('app', async () => {
        await run('npm',['run','watch'])
    })
}



// ============
// === Dist ===
// ============

async function dist_rust () {
    await build_rust()
}

async function dist_js () {
    await cmd.with_cwd('app', async () => {
        await run('npm',['run','dist'])
    })
}



// ============
// === Main ===
// ============

async function main () {
    let command = argv._[0]

    let do_rust = (argv.rust == true) || (argv.rust == undefined)
    let do_js   = (argv.js   == true) || (argv.js   == undefined)

    if (command == 'clean') {
        cmd.section('Cleaning')
        if (do_rust) { await clean_rust() }
        if (do_js)   { await clean_js() }
        return
    }

    if (command == 'watch') {
        cmd.section('Watching')
        let promises = []
        if (do_rust) { promises.push(watch_rust()) }
        if (do_js)   { promises.push(watch_js()) }
        await Promise.all(promises)
        return
    }

    if (command == 'build') {
        cmd.section('Building')
        if (do_rust) { await build_rust() }
        if (do_js)   { await build_js() }
        return
    }

    if (command == 'check') {
        cmd.section('Checking')
        if (do_rust) { await check_rust() }
        if (do_js)   { await check_js() }
        return
    }

    if (command == 'lint') {
        cmd.section('Linting')
        if (do_rust) { await lint_rust() }
        if (do_js)   { await lint_js() }
        return
    }

    if (command == 'dist') {
        cmd.section('Packaging')
        await dist_rust()
        await dist_js()
        return
    }

    print_help()
}

main()
