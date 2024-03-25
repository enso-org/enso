import { setupGatewayClient } from './ydoc'

const wss = new WebSocketServer({ host: 'localhost', port: 1234 })

wss.onconnect = (socket, url) => setupGatewayClient(socket, "ws://localhost:8080", url)

wss.start()
