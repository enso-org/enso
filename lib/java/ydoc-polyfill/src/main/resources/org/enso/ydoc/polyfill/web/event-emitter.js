(function (jvm) {

    class EventEmitter extends EventTarget {

        #store;

        constructor() {
            super();

            this.#store = jvm('new-event-store');
        }

        listeners(eventName) {
            return jvm('get-listeners', this.#store, eventName);
        }

        on(eventName, listener) {
            jvm('add-listener', this.#store, eventName, listener);
        }

        addListener(eventName, listener) {
            this.on(eventName, listener);
        }

        off(eventName, listener) {
            jvm('remove-listener', this.#store, eventName, listener);
        }

        removeListener(eventName, listener) {
            this.off(eventName, listener);
        }

        emit(eventName, ...args) {
            jvm('emit', this.#store, eventName, args);
        }

    }

    globalThis.EventEmitter = EventEmitter;

})
