(function (jvm) {

    //
    // EventBase
    //

    var EventBase = {
        bubbles: false,
        cancelable: false
    };

    //
    // WebSocket
    //

    var WebSocket = (function () {

        function WebSocket(url, protocols) {
            this.state_ = WebSocket.CONNECTING;

            this.url_ = url;

            if (typeof protocols === "string") {
                protocols = [protocols];
            }
            this.protocols_ = protocols;

            if (url == null && protocols == null) {
                this.connection_ = jvm(
                    'new-web-socket-connection',
                    this._handle_open.bind(this),
                    this._handle_close.bind(this),
                    this._handle_error.bind(this),
                    this._handle_message.bind(this)
                );
            } else {
                this.connection_ = jvm(
                    'new-web-socket',
                    url,
                    protocols,
                    this._handle_open.bind(this),
                    this._handle_close.bind(this),
                    this._handle_error.bind(this),
                    this._handle_message.bind(this)
                );
            }
        }

        //
        // Constants
        //

        WebSocket.CONNECTING = 0;
        WebSocket.OPEN = 1;
        WebSocket.CLOSING = 2;
        WebSocket.CLOSED = 3;

        //
        // Properties
        //

        Object.defineProperty(WebSocket.prototype, "url", {
            get: function () {
                return this.url_;
            },
            enumerable: false,
            configurable: true
        });

        Object.defineProperty(WebSocket.prototype, "protocol", {
            get: function () {
                return this.protocols_ ? this.protocols_[0] : '';
            },
            enumerable: false,
            configurable: true
        });

        Object.defineProperty(WebSocket.prototype, "readyState", {
            get: function () {
                return this.state_;
            },
            enumerable: false,
            configurable: true
        });

        Object.defineProperty(WebSocket.prototype, "binaryType", {
            value: "arraybuffer",
            writable: true,
            enumerable: false,
            configurable: true
        });

        //
        // Listeners
        //

        Object.defineProperty(WebSocket.prototype, "onopen", {
            get: function () {
                return jvm('get-on-listener', this.connection_, 'open');
            },
            set: function (listener) {
                jvm('set-on-listener', this.connection_, 'open', listener);
            },
            enumerable: false,
            configurable: true
        });

        Object.defineProperty(WebSocket.prototype, "onclose", {
            get: function () {
                return jvm('get-on-listener', this.connection_, 'close');
            },
            set: function (listener) {
                jvm('set-on-listener', this.connection_, 'close', listener);
            },
            enumerable: false,
            configurable: true
        });

        Object.defineProperty(WebSocket.prototype, "onmessage", {
            get: function () {
                return jvm('get-on-listener', this.connection_, 'message');
            },
            set: function (listener) {
                jvm('set-on-listener', this.connection_, 'message', listener);
            },
            enumerable: false,
            configurable: true
        });

        Object.defineProperty(WebSocket.prototype, "onerror", {
            get: function () {
                return jvm('get-on-listener', this.connection_, 'error');
            },
            set: function (listener) {
                jvm('set-on-listener', this.connection_, 'error', listener);
            },
            enumerable: false,
            configurable: true
        });


        //
        // Methods
        //

        WebSocket.prototype.send = function (data) {
            if (typeof data.valueOf() === 'string') {
                jvm('web-socket-send-text', this.connection_, data);
            } else {
                jvm('web-socket-send-binary', this.connection_, data);
            }
        };

        WebSocket.prototype.close = function (code, reason) {
            this.state_ = WebSocket.CLOSING;
            if (code === undefined) {
                jvm('web-socket-terminate', this.connection_);
            } else {
                jvm('web-socket-close', this.connection_, code, reason);
            }
        }

        //
        // EventTarget
        //

        WebSocket.prototype.addEventListener = function (type, listener) {
            jvm('add-event-listener', this.connection_, type, listener);
        };

        WebSocket.prototype.removeEventListener = function (type, listener) {
            jvm('remove-event-listener', this.connection_, type, listener);
        };

        WebSocket.prototype.dispatchEvent = function (event) {
            event.target = this;
            // event.timeStamp = ...

            jvm('dispatch-event', this.connection_, event.type, event);
        };

        //
        // EventEmitter
        //

        WebSocket.prototype.on = function(type, listener) {
            this.addEventListener(type, event => {
                switch (type) {
                    case 'message':
                        listener(event.data);
                        break;
                    default:
                        listener(event);
                }
            })
        };

        //
        // Session callbacks
        //

        WebSocket.prototype._handle_open = function () {
            this.state_ = WebSocket.OPEN;
            this.dispatchEvent({
                ...EventBase,
                type: 'open'
            });
        };

        WebSocket.prototype._handle_close = function (code, reason) {
            this.state_ = WebSocket.CLOSED;
            this.dispatchEvent({
                ...EventBase,
                type: 'close',
                code: code,
                reason: reason,
                wasClean: true
            });
        };

        WebSocket.prototype._handle_error = function (message) {
            this.dispatchEvent({
                ...EventBase,
                type: 'error',
                message: message
            });
        };

        WebSocket.prototype._handle_message = function (data) {
            this.dispatchEvent({
                ...EventBase,
                type: 'message',
                data: data
            });
        };

        return WebSocket;
    }());

    //
    // WebSocketServer
    //

    var WebSocketServer = (function () {

        function WebSocketServer(config) {
            this.config_ = config;

            this.server_ = jvm(
                'new-web-socket-server',
                config.host,
                config.port,
                this._handle_connect.bind(this)
            );
        }

        WebSocketServer.prototype.start = function() {
            jvm('web-socket-server-start', this.server_);
        }

        WebSocketServer.prototype.onconnect = function(socket, url) {};

        //
        // Connection callbacks
        //

        WebSocketServer.prototype._handle_connect = function () {
            var ws = new WebSocket(null, null);

            ws.addEventListener('open', () => this.onconnect(ws, '/'));

            return ws.connection_;
        };

        return WebSocketServer;
    }());

    globalThis.WebSocket = WebSocket;

    globalThis.WebSocketServer = WebSocketServer;

})
