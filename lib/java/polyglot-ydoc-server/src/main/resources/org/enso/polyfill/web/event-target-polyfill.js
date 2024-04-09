(function (jvm) {

    class EventTarget {

        #eventStore;

        constructor() {
            this.#eventStore = jvm('new-event-target');
        }

        addEventListener(type, listener) {
            jvm('add-event-listener', this.#eventStore, type, listener);
        };

        removeEventListener(type, listener) {
            jvm('remove-event-listener', this.#eventStore, type, listener);
        };

        dispatchEvent(event) {
            event.target = this;

            jvm('dispatch-event', this.#eventStore, event.type, event);
        };
    }

    globalThis.EventTarget = EventTarget;

})
