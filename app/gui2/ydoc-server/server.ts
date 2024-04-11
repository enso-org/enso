import { setupGatewayClient } from './ydoc'

declare global {
    class WebSocketServer {
        constructor(config: any)
        onconnect: ((socket: any, url: any) => any) | null;
        start(): void;
    }
}

const wss = new WebSocketServer({ host: 'localhost', port: 1234 })

wss.onconnect = (socket, url) => setupGatewayClient(socket, "ws://127.0.0.1:30616", 'index')

wss.start()
