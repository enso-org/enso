(function (jvm) {

    globalThis.parse_module = function(code) {
        const byteBuffer = jvm('parse-module', code);
        return new Uint8Array(new ArrayBuffer(byteBuffer));
    };

    globalThis.parse_block = function(code) {
        const byteBuffer = jvm('parse-block', code);
        return new Uint8Array(new ArrayBuffer(byteBuffer));
    };

    globalThis.xxHash128 = function(input) {
        return jvm('xx-hash-128', input);
    };

    globalThis.is_ident_or_operator = function(code) {
        return jvm('is-ident-or-operator', code);
    };
})
