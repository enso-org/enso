(function (jvm) {

    class Buffer {

        #buffer;
        #offset;
        #length;

        constructor(buffer, offset, length) {
            this.#buffer = buffer;
            this.#offset = offset;
            this.#length = length;
        }

        get buffer() {
            if (this.#offset) {
                const len = this.#length ?? this.#buffer.byteLength - this.#offset;
                return this.#buffer.slice(this.#offset, this.#offset + len);
            } else {
                return this.#buffer;
            }
        }

        static from(a1, a2, a3) {
            if (a1 instanceof ArrayBuffer) {
                return new Buffer(a1, a2, a3)
            } else {
                const txt = a1;
                const encoding = a2;
                return new Buffer(jvm('buffer-from', txt, encoding));
            }
        }

        toString(encoding) {
            return jvm('buffer-to-string', this.buffer, encoding);
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
