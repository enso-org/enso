import { configureAllDebugLogs, docName, setupGatewayClient } from 'ydoc-server'

const host = typeof YDOC_HOST == 'string' ? YDOC_HOST : 'localhost'
const port = typeof YDOC_PORT == 'number' ? YDOC_PORT : 1234
const debug = typeof YDOC_LS_DEBUG != 'undefined'

configureAllDebugLogs(debug)

const wss = new WebSocketServer({ host, port })

wss.onconnect = (socket, url) => {
  const doc = docName(url.pathname)
  const ls = url.searchParams.get('ls')
  if (doc != null && ls != null) {
    console.log('setupGatewayClient', ls, doc)
    setupGatewayClient(socket, ls, doc)
  } else {
    console.log('Failed to authenticate user', ls, doc)
  }
}

wss.start()
