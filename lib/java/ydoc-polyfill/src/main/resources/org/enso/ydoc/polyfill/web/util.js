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

    class TextEncoder {

        get encoding() {
            return 'utf-8';
        }

        encode(input) {
            const text = input === undefined ? '' : input;
            const buffer = jvm('text-encoder-encode', text);

            return new Uint8Array(new ArrayBuffer(buffer));
        }
    }

    globalThis.TextDecoder = TextDecoder;

    globalThis.TextEncoder = TextEncoder;

})
