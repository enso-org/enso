(function (jvm) {

    class Buffer {

        #buffer;

        constructor(buffer) {
            this.#buffer = buffer;
        }

        get buffer() {
            return this.#buffer;
        }

        static from(txt, encoding) {
            return new Buffer(jvm('buffer-from', txt, encoding));
        }

        toString(encoding) {
            return jvm('buffer-to-string', this.#buffer, encoding);
        }
    }

    class Zlib {

        deflateSync(buffer) {
            const result = jvm('zlib-deflate-sync', buffer.buffer);
            return new Buffer(result);
        }

        inflateSync(buffer) {
            const result = jvm('zlib-inflate-sync', buffer.buffer);
            return new Buffer(result);
        }
    }

    globalThis.Buffer = Buffer;
    globalThis.zlib = new Zlib();

})
