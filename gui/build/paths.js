const path  = require('path')



// =============
// === Paths ===
// =============

let paths  = {}

paths.root                = path.dirname(__dirname)

paths.script              = {}
paths.script.main         = path.join(paths.root,'run')
paths.script.root         = path.join(paths.root,'build')
paths.script.run          = path.join(paths.script.root,'run')

paths.dist                = {}
paths.dist.root           = path.join(paths.root,'dist')
paths.dist.client         = path.join(paths.dist.root,'client')
paths.dist.content        = path.join(paths.dist.root,'content')
paths.dist.init           = path.join(paths.dist.root,'init')
paths.dist.buildInfo      = path.join(paths.dist.root,'build.json')

paths.dist.wasm           = {}
paths.dist.wasm.root      = path.join(paths.dist.root,'wasm')
paths.dist.wasm.main      = path.join(paths.dist.wasm.root,'gui.wasm')
paths.dist.wasm.mainRaw   = path.join(paths.dist.wasm.root,'gui_bg.wasm')
paths.dist.wasm.glue      = path.join(paths.dist.wasm.root,'gui.js')
paths.dist.wasm.mainOpt   = path.join(paths.dist.wasm.root,'gui_opt.wasm')
paths.dist.wasm.mainOptGz = path.join(paths.dist.wasm.root,'gui_opt.wasm.gz')

paths.js                  = {}
paths.js.root             = path.join(paths.root,'src','js')

paths.rust                = {}
paths.rust.root           = path.join(paths.root,'src','rust')

module.exports = paths
