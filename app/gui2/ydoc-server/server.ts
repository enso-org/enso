import { setupGatewayClient } from './ydoc'

const wss = new WebSocketServer({ host: 'localhost', port: 1234 })

wss.onconnect = (socket, url) => setupGatewayClient(socket, "language-server-url", url)

wss.start()
