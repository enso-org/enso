/** @file entrypoint for standalone ydoc-server running in nodejs environment. */

import './cjs-shim' // must be imported first

const DEFAULT_PORT = 5976
const PORT = (process.env.PORT != null && parseInt(process.env.PORT, 10)) || DEFAULT_PORT
const HOSTNAME = process.env.GUI_HOSTNAME ?? 'localhost'
const LANGUAGE_SERVER_URL = process.env.LANGUAGE_SERVER_URL
