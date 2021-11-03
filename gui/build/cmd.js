const https = require('https')
const spawn = require('child_process').spawn
const exec = require('child_process').exec

function download(url) {
    return new Promise((resolve, reject) => {
        https
            .get(url, res => {
                let data = ''
                res.on('data', chunk => (data += chunk))
                res.on('end', () => resolve(data))
            })
            .on('error', error => reject(error))
    })
}

function section(title) {
    let border = '-'.repeat(8 + title.length)
    let middle = '--- ' + title + ' ---'
    console.log()
    console.log()
    console.log()
    console.log(border)
    console.log(middle)
    console.log(border)
    console.log()
}

async function with_cwd(dir, fn) {
    let cwd = process.cwd()
    process.chdir(dir)
    let out = await fn()
    process.chdir(cwd)
    return out
}

function run(cmd, args) {
    let out = ''
    return new Promise((resolve, reject) => {
        console.log(`Calling '${cmd} ${args.join(' ')}'`)
        let proc = spawn(cmd, args, { stdio: 'inherit', shell: true })
        proc.on('exit', code => {
            if (code) process.exit(code)
            resolve(out)
        })
    })
}

function run_read(cmd, args) {
    let out = ''
    return new Promise((resolve, reject) => {
        let proc = spawn(cmd, args, { shell: true })
        proc.stderr.pipe(process.stderr)
        proc.stdout.on('data', data => {
            out += data
        })
        proc.on('exit', code => {
            if (code) process.exit(code)
            resolve(out)
        })
    })
}

async function check_version(name, required, cfg) {
    if (!cfg) {
        cfg = {}
    }
    let version = await run_read(name, ['--version'])
    version = version.trim()
    if (cfg.preprocess) {
        version = cfg.preprocess(version)
    }
    if (cfg.silent !== true) {
        console.log(`Checking if '${name}' version is '${required}'.`)
    }
    if (version != required) {
        throw `[ERROR] The '${name}' version '${version}' does not match the required one '${required}'.`
    }
}

async function get_npm_info(name) {
    let info = await run_read('npm', ['info', name, '--json'])
    return JSON.parse(info)
}

async function get_npm_lts_version_of(name) {
    let info = await get_npm_info(name)
    version = info['dist-tags'].lts
    return version
}

async function get_node_dist_index() {
    let index = await download('https://nodejs.org/dist/index.json')
    return JSON.parse(index)
}

async function get_node_lts_version() {
    let index = await get_node_dist_index()
    let newest = null
    for (let entry of index) {
        if (entry.lts !== false) {
            newest = entry
            break
        }
    }
    if (!newest) {
        throw 'Cannot fetch the info about node LTS version.'
    }
    let node = newest.version
    let npm = newest.npm
    return [node, npm]
}

module.exports = {
    section,
    run,
    run_read,
    check_version,
    get_npm_info,
    get_npm_lts_version_of,
    with_cwd,
    get_node_dist_index,
    get_node_lts_version,
}
