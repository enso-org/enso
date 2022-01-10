const path = require('path')
const os = require('os')

// =============
// === Paths ===
// =============

let paths = {}

paths.root = path.dirname(__dirname)

paths.github = {}
paths.github.root = path.join(paths.root, '.github')
paths.github.workflows = path.join(paths.github.root, 'workflows')

paths.script = {}
paths.script.main = path.join(paths.root, 'run')
paths.script.root = path.join(paths.root, 'build')
paths.script.run = path.join(paths.script.root, 'run')

paths.dist = {}
paths.dist.root = path.join(paths.root, 'dist')
paths.dist.client = path.join(paths.dist.root, 'client')
paths.dist.content = path.join(paths.dist.root, 'content')
paths.dist.assets = path.join(paths.dist.content, 'assets')
paths.dist.packageJson = path.join(paths.dist.content, 'package.json')
paths.dist.preload = path.join(paths.dist.content, 'preload.js')
paths.dist.bin = path.join(paths.dist.root, 'bin')
paths.dist.init = path.join(paths.dist.root, 'init')
paths.dist.buildInit = path.join(paths.dist.root, 'build-init')
paths.dist.buildInfo = path.join(paths.dist.root, 'build.json')
paths.dist.tmp = path.join(paths.dist.root, 'tmp')

const WASM_MAIN = 'ide.wasm'
const WASM_MAIN_RAW = 'ide_bg.wasm'
const WASM_GLUE = 'ide.js'

// Final WASM artifacts in `dist` directory.
paths.dist.wasm = {}
paths.dist.wasm.root = path.join(paths.dist.root, 'wasm')
paths.dist.wasm.main = path.join(paths.dist.wasm.root, WASM_MAIN)
paths.dist.wasm.mainRaw = path.join(paths.dist.wasm.root, WASM_MAIN_RAW)
paths.dist.wasm.glue = path.join(paths.dist.wasm.root, WASM_GLUE)

// Intermediate WASM artifacts.
paths.wasm = {}
paths.wasm.root = path.resolve(os.tmpdir(), 'enso-wasm')
paths.wasm.main = path.join(paths.wasm.root, WASM_MAIN)
paths.wasm.mainRaw = path.join(paths.wasm.root, WASM_MAIN_RAW)
paths.wasm.glue = path.join(paths.wasm.root, WASM_GLUE)
paths.wasm.mainGz = path.join(paths.wasm.root, 'ide.wasm.gz')

paths.ide_desktop = {}
paths.ide_desktop.lib = {}
paths.ide_desktop.root = path.join(paths.root, 'app', 'ide-desktop')
paths.ide_desktop.lib.projectManager = path.join(paths.ide_desktop.root, 'lib', 'project-manager')
paths.ide_desktop.lib.content = path.join(paths.ide_desktop.root, 'lib', 'content')

paths.gui = {}
paths.gui.root = path.join(paths.root, 'app', 'gui')

function get_project_manager_extension() {
    const target_platform = os.platform()
    switch (target_platform) {
        case 'win32':
            return '.exe'
        default:
            return ''
    }
}

paths.get_project_manager_path = function (root) {
    let base_path = path.join(root, 'enso', 'bin')
    const extension = get_project_manager_extension()
    return path.join(base_path, 'project-manager') + extension
}

module.exports = paths
