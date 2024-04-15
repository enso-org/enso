(function (jvm) {

    //
    // EventBase
    //

    const EventBase = {
        bubbles: false,
        cancelable: false
    };

    //
    // WebSocket
    //

    class WebSocket extends EventEmitter {

        //
        // Constants
        //

        static CONNECTING = 0;
        static OPEN = 1;
        static CLOSING = 2;
        static CLOSED = 3;

        //
        // State
        //

        #state;
        #url;
        #protocols;

        _connection;

        //
        // Constructor
        //

        constructor(url, protocols) {
            super();

            this.#state = WebSocket.CONNECTING;

            this.#url = url;

            if (typeof protocols === "string") {
                protocols = [protocols];
            }
            this.#protocols = protocols;

            if (url == null && protocols == null) {
                this._connection = jvm(
                    'new-web-socket-connection',
                    this.#handle_open.bind(this),
                    this.#handle_close.bind(this),
                    this.#handle_error.bind(this),
                    this.#handle_message.bind(this)
                );
            } else {
                this._connection = jvm(
                    'new-web-socket',
                    url,
                    protocols,
                    this.#handle_open.bind(this),
                    this.#handle_close.bind(this),
                    this.#handle_error.bind(this),
                    this.#handle_message.bind(this)
                );
            }
        }

        //
        // Properties
        //

        get url() {
            return this.#url;
        }

        get protocol() {
            return this.#protocols ? this.#protocols[0] : '';
        }

        get readyState() {
            return this.#state;
        }

        get binaryType() {
            return "arraybuffer";
        }

        //
        // Listeners
        //

        get onopen() {
            return this.getEventListeners('open').slice(-1)[0];
        }

        set onopen(listener) {
            const listeners = this.getEventListeners('open');
            if (!listeners.some((value) => value === listener)) {
                this.addEventListener('open', listener);
            }
        }

        get onclose() {
            return this.getEventListeners('close').slice(-1)[0];
        }

        set onclose(listener) {
            const listeners = this.getEventListeners('close');
            if (!listeners.some((value) => value === listener)) {
                this.addEventListener('close', listener);
            }
        }

        get onmessage() {
            return this.getEventListeners('message').slice(-1)[0];
        }

        set onmessage(listener) {
            const listeners = this.getEventListeners('message');
            if (!listeners.some((value) => value === listener)) {
                this.addEventListener('message', listener);
            }
        }

        get onerror() {
            return this.getEventListeners('error').slice(-1)[0];
        }

        set onerror(listener) {
            const listeners = this.getEventListeners('error');
            if (!listeners.some((value) => value === listener)) {
                this.addEventListener('error', listener);
            }
        }

        //
        // Methods
        //

        send(data) {
            if (typeof data.valueOf() === 'string') {
                jvm('web-socket-send-text', this._connection, data);
            } else {
                jvm('web-socket-send-binary', this._connection, data);
            }
        }

        close(code, reason) {
            this.#state = WebSocket.CLOSING;
            if (code === undefined) {
                jvm('web-socket-terminate', this._connection);
            } else {
                jvm('web-socket-close', this._connection, code, reason);
            }
            this.#state = WebSocket.CLOSED;
        }

        //
        // Session callbacks
        //

        #handle_open() {
            this.#state = WebSocket.OPEN;
            this.dispatchEvent({
                ...EventBase,
                type: 'open'
            });
            this.emit('open');
        }

        #handle_close(code, reason) {
            this.#state = WebSocket.CLOSED;
            this.dispatchEvent({
                ...EventBase,
                type: 'close',
                code: code,
                reason: reason,
                wasClean: true
            });
            this.emit('close', code, reason);
        }

        #handle_error(message) {
            this.dispatchEvent({
                ...EventBase,
                type: 'error',
                message: message
            });
            this.emit('error', message);
        }

        #handle_message(data) {
            this.dispatchEvent({
                ...EventBase,
                type: 'message',
                data: data
            });
            this.emit('message', data);
        }
    }

    //
    // WebSocketServer
    //

    class WebSocketServer {

        #config;
        #server;

        onconnect = function(socket, url) {};

        constructor(config) {
            this.#config = config;

            this.#server = jvm(
                'new-web-socket-server',
                config.host,
                config.port,
                this.#handle_connect.bind(this)
            );
        }

        start() {
            jvm('web-socket-server-start', this.#server);
        }

        //
        // Connection callbacks
        //

        #handle_connect() {
            var ws = new WebSocket(null, null);

            ws.addEventListener('open', () => this.onconnect(ws, '/'));

            return ws._connection;
        }
    }

    globalThis.WebSocket = WebSocket;

    globalThis.WebSocketServer = WebSocketServer;

})
