import * as wasm_rust_glue from 'wasm_rust_glue'

/// WARNING
/// This module is a hacky binding to wasm_pack. It works only if the wasm_pack output is
/// preprocessed by the build scripts. See the following link to learn more:
/// https://github.com/rustwasm/wasm-pack/issues/790

exports.wasm_imports = function () {
    return wasm_rust_glue.default()
}

exports.after_load = function (w, module) {
    return wasm_rust_glue.after_load(w, module)
}
