(function (jvm) {

    class Event {

        #type;
        #cancelable = false;
        #timeStamp;

        constructor(type, options) {
            this.#type = type;

            if (typeof options === 'object') {
                this.#cancelable = options.cancelable || false;
            }

            this.#timeStamp = performance.now();
        }

        get bubbles() {
            return false;
        }

        get cancelable() {
            return this.#cancelable;
        }

        get composed() {
            return false;
        }

        get timeStamp() {
            return this.#timeStamp();
        }

        get type() {
            return this.#type;
        }
    }

    class EventTarget {

        #eventStore;

        constructor() {
            this.#eventStore = jvm('new-event-target');
        }

        getEventListeners(type) {
            return jvm('get-event-listeners', this.#eventStore, type);
        }

        addEventListener(type, listener) {
            jvm('add-event-listener', this.#eventStore, type, listener);
        }

        removeEventListener(type, listener) {
            jvm('remove-event-listener', this.#eventStore, type, listener);
        }

        dispatchEvent(event) {
            event.target = this;

            jvm('dispatch-event', this.#eventStore, event.type, event);
        }
    }

    globalThis.Event = Event;

    globalThis.EventTarget = EventTarget;

})
