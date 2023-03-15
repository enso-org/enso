/** @file This file is injected into every entry point, but it needs to run only once. A global variable is used to
 * ensure that. */
if (!window.liveReloadListening) {
    window.liveReloadListening = true
    const protocol = window.location.protocol === 'http:' ? 'ws://' : 'wss://'
    const address = protocol + window.location.host + '/live-reload'
    const socket = new WebSocket(address)
    socket.onmessage = msg => {
        if (msg.data === 'reload') {
            window.location.reload()
        }
    }
}
