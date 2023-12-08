import * as fsSync from 'node:fs'
import * as fs from 'node:fs/promises'
import * as https from 'node:https'
import tar from 'tar'
import bz2 from 'unbzip2-stream'

const ENSO_FONT_URL = 'https://github.com/enso-org/font/releases/download/1.0/enso-font-1.0.tar.gz'
const MPLUS1_FONT_URL =
  'https://github.com/coz-m/MPLUS_FONTS/raw/71d438c798d063cc6fdae8d2864bc48f2d3d06ad/fonts/ttf/MPLUS1%5Bwght%5D.ttf'
const DEJAVU_SANS_MONO_FONT_URL =
  'https://sourceforge.net/projects/dejavu/files/dejavu/2.37/dejavu-fonts-ttf-2.37.tar.bz2'

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
await fs.rm('./public/font-enso/', { recursive: true, force: true })
await fs.mkdir('./public/font-enso/', { recursive: true })
await new Promise((resolve, reject) => {
  httpsGet(ENSO_FONT_URL, (response) => {
    response.pipe(
      tar.extract({
        cwd: './public/font-enso/',
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
console.info('Downloading M PLUS 1 font...')
await fs.rm('./public/font-mplus1/', { recursive: true, force: true })
await fs.mkdir('./public/font-mplus1/', { recursive: true })
await new Promise((resolve, reject) => {
  httpsGet(MPLUS1_FONT_URL, (response) => {
    response.pipe(fsSync.createWriteStream('./public/font-mplus1/MPLUS1.ttf'))
    response.on('end', resolve)
    response.on('error', reject)
  })
})
console.info('Downloading DejaVu Sans Mono font...')
await fs.rm('./public/font-dejavu/', { recursive: true, force: true })
await fs.mkdir('./public/font-dejavu/', { recursive: true })
await new Promise((resolve, reject) => {
  httpsGet(DEJAVU_SANS_MONO_FONT_URL, (response) => {
    response.pipe(bz2()).pipe(
      tar.extract({
        cwd: './public/font-dejavu/',
        strip: 2,
        filter(path) {
          return /[\\/]DejaVuSansMono/.test(path) || !/Oblique[.]ttf$/.test(path)
        },
      }),
    )
    response.on('end', resolve)
    response.on('error', reject)
  })
})
console.info('Done.')
