import * as fs from 'node:fs/promises'
import * as https from 'node:https'
import tar from 'tar'

const ENSO_FONT_URL = 'https://github.com/enso-org/font/releases/download/1.0/enso-font-1.0.tar.gz'

/** @param {string | https.RequestOptions | URL} options
 * @param {((res: import('node:http').IncomingMessage) => void) | undefined} [callback] */
function httpsGet(options, callback) {
  return https.get(options, (response) => {
    const location = response.headers.location
    if (location) {
      httpsGet(
        typeof options === 'string' || options instanceof URL
          ? location
          : { ...options, ...new URL(location) },
        callback,
      )
    } else {
      callback?.(response)
    }
  })
}

console.info('Downloading Enso font...')
await fs.rm('./public/enso-font/', { recursive: true, force: true })
await fs.mkdir('./public/enso-font/', { recursive: true })
await new Promise((resolve, reject) => {
  httpsGet(ENSO_FONT_URL, (response) => {
    response.pipe(
      tar.extract({
        cwd: './public/enso-font/',
        strip: 1,
        filter(path) {
          // Reject files starting with `.`.
          return !/[\\/][.]/.test(path)
        },
      }),
    )
    response.on('end', resolve)
    response.on('error', reject)
  })
})
console.info('Done.')
