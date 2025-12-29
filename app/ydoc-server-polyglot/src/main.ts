import { configureAllDebugLogs, docName, setupGatewayClient } from 'ydoc-server'

const host = typeof YDOC_HOST == 'string' ? YDOC_HOST : 'localhost'
const port = typeof YDOC_PORT == 'number' ? YDOC_PORT : 1234
const debug = typeof YDOC_LS_DEBUG != 'undefined'

configureAllDebugLogs(debug)

if (YDOC_MESSAGE_CALLBACKS == undefined) {
  throw new Error('YDOC_MESSAGE_CALLBACKS undefined')
}

const wss = new WebSocketServer({ host, port })

wss.onconnect = (socket, url) => {
  const doc = docName(url.pathname)
  const ls = url.searchParams.get('ls')
  const data = url.searchParams.get('data')
  if (doc != null && ls != null) {
    setupGatewayClient(socket, ls, data, doc, YDOC_MESSAGE_CALLBACKS)
  } else {
    console.log('Failed to authenticate user', ls, doc)
  }
}

wss.start()
