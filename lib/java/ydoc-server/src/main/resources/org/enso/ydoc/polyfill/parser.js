(function (jvm) {

    globalThis.parse_tree = function(code) {
        const byteBuffer = jvm('parse-tree', code);
        return new Uint8Array(new ArrayBuffer(byteBuffer));
    };

    globalThis.xxHash128 = function(input) {
        return jvm('xx-hash-128', input);
    };

    globalThis.is_ident_or_operator = function(code) {
        return jvm('is-ident-or-operator', code);
    };
})
