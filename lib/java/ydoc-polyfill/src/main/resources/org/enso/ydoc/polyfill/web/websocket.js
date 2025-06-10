(function (jvm) {

    const EMPTY_ARRAY = new Uint8Array(new ArrayBuffer(0));

    //
    // Events
    //

    class CloseEvent extends Event {

        #code;
        #reason;

        constructor(type, options) {
            super(type);

            if (typeof options === 'object') {
                this.#code = options.code;
                this.#reason = options.reason;
            }
        }

        get code() {
            return this.#code;
        }

        get reason() {
            return this.#reason;
        }

        get wasClean() {
            return true;
        }
    }

    class MessageEvent extends Event {

        #data;
        #origin;
        #lastEventId;
        #source;
        #ports;

        constructor(type, options) {
            super(type);

            if (typeof options === 'object') {
                this.#data = options.data;
                this.#origin = options.origin;
                this.#lastEventId = options.lastEventId;
                this.#source = options.source;
                this.#ports = options.ports;
            }
        }

        get data() {
            return this.#data;
        }

        get origin() {
            return this.#origin;
        }

        get lastEventId() {
            return this.#lastEventId;
        }

        get source() {
            return this.#source;
        }

        get ports() {
            return this.#ports;
        }
    }

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

        #binaryType;
        #state;
        #url;
        #protocols;

        _connection;

        //
        // Constructor
        //

        constructor(url, protocols) {
            super();

            this.#binaryType = "arraybuffer";
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
                    this.#handle_message.bind(this),
                    this.#handle_ping.bind(this),
                    this.#handle_pong.bind(this),
                    this.#handle_upgrade.bind(this)
                );
            } else {
                this._connection = jvm(
                    'new-web-socket',
                    url,
                    protocols,
                    this.#handle_open.bind(this),
                    this.#handle_close.bind(this),
                    this.#handle_error.bind(this),
                    this.#handle_message.bind(this),
                    this.#handle_ping.bind(this),
                    this.#handle_pong.bind(this),
                    this.#handle_upgrade.bind(this)
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
            return this.#binaryType;
        }

        set binaryType(value) {
            this.#binaryType = value;
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
            if (typeof data === 'string') {
                jvm('web-socket-send-text', this._connection, data);
            } else {
                jvm('web-socket-send-binary', this._connection, data.buffer, data.byteOffset, data.byteLength);
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

        ping(data) {
            const arr = data || EMPTY_ARRAY;
            jvm('web-socket-ping', this._connection, arr.buffer, arr.byteOffset, arr.byteLength);
        }

        pong(data) {
            const arr = data || EMPTY_ARRAY;
            jvm('web-socket-pong', this._connection, arr.buffer, arr.byteOffset, arr.byteLength);
        }

        //
        // Session callbacks
        //

        #handle_open() {
            this.#state = WebSocket.OPEN;
            this.dispatchEvent(new Event('open'));
            this.emit('open');
        }

        #handle_close(code, reason) {
            this.#state = WebSocket.CLOSED;
            this.dispatchEvent(new CloseEvent('close', { code, reason }));
            this.emit('close', code, reason);
        }

        #handle_error(message) {
            this.dispatchEvent(new Event('error'));
            this.emit('error', message);
        }

        #handle_message(data) {
            this.dispatchEvent(new MessageEvent('message', { data }));
            this.emit('message', data);
        }

        #handle_ping(data) {
            this.emit('ping', data);
        }

        #handle_pong(data) {
            this.emit('pong', data);
        }

        #handle_upgrade(path) {
            this.emit('upgrade', path);
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
            const ws = new WebSocket(null, null);
            ws.on('upgrade', (url) => this.onconnect(ws, url));

            return ws._connection;
        }
    }

    globalThis.WebSocket = WebSocket;

    globalThis.WebSocketServer = WebSocketServer;

})
