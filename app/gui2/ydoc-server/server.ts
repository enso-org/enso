import { setupGatewayClient } from './ydoc'

declare global {
    class WebSocketServer {
        constructor(config: any)
        onconnect: ((socket: any, url: any) => any) | null;
        start(): void;
    }
}

const wss = new WebSocketServer({ host: 'localhost', port: 1234 })

wss.onconnect = (socket, url) => setupGatewayClient(socket, "ws://localhost:8080", url)

wss.start()
