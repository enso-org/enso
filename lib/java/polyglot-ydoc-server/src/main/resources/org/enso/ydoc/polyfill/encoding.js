(function (jvm) {

    class TextDecoder {

        constructor(encoding) {
            if (typeof encoding === 'string') {
                this._encoding = encoding;
            }
        }

        decode(arr) {
            return jvm('text-decoder-decode', this._encoding, arr);
        }
    }

    globalThis.TextDecoder = TextDecoder;

})
