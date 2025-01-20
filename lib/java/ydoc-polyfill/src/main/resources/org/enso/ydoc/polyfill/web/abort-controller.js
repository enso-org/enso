(function (jvm) {

    const ABORT_ERR = { code: 20, name: "AbortError" };

    class AbortSignal extends EventTarget {

        #aborted;
        #reason;

        constructor(aborted, reason) {
            super();
            this.#aborted = aborted === undefined ? false : aborted;
            this.#reason = reason === undefined ? ABORT_ERR : reason;
        }

        get aborted() {
            return this.#aborted;
        }

        set aborted(value) {
            this.#aborted = value === undefined ? false : value;
        }

        get reason() {
            return this.#reason;
        }

        set reason(value) {
            const reasonValue = value === undefined ? ABORT_ERR : value;
            this.#reason = reasonValue;
        }

        set onabort(callback) {
            this.addEventListener('abort', callback);
        }

        static abort(reason) {
            return new AbortSignal(true, reason);
        }

        throwIfAborted() {
            if (this.#aborted) {
                throw this.#reason;
            }
        }
    }

    class AbortController {

        #signal;

        constructor() {
            const signal = new AbortSignal();
            signal.addEventListener('abort', (event) => {
                signal.aborted = true;
                signal.reason = event.reason;
            })
            this.#signal = signal;
        }

        get signal() {
            return this.#signal;
        }

        abort(reason) {
            this.#signal.dispatchEvent({type: 'abort', reason: reason});
        }
    }

    globalThis.AbortController = AbortController;

})
