import { docName } from './auth'
import { setupGatewayClient } from './ydoc'

declare global {
    class WebSocketServer {
        constructor(config: any)
        onconnect: ((socket: any, url: any) => any) | null;
        start(): void;
    }
}

const wss = new WebSocketServer({ host: 'localhost', port: 1234 })

wss.onconnect = (socket, url) => {
    const doc = docName(url.pathname)
    const ls = url.searchParams.get('ls')
    if (doc != null && ls != null) {
        console.log("setupGatewayClient ", ls, doc)
        setupGatewayClient(socket, ls, doc)
    } else {
        console.log('Failed to authenticate user', ls, doc)
    }
}

wss.start()
