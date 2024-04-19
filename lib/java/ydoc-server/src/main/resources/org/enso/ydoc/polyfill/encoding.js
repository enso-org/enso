(function (jvm) {

    class TextDecoder {

        #encoding;

        constructor(encoding) {
            if (typeof encoding === 'string') {
                this.#encoding = encoding;
            }
        }

        decode(arr) {
            return jvm('text-decoder-decode', this.#encoding, arr.buffer, arr.byteOffset, arr.byteLength);
        }
    }

    globalThis.TextDecoder = TextDecoder;

})
