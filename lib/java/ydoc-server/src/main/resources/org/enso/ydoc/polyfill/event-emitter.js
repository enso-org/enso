(function (jvm) {

    class EventEmitter {

        #store;

        constructor() {
            this.#store = jvm('new-event-store');
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
