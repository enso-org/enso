/** @file ⚠️⚠️⚠️ THIS SCRIPT IS PROVIDED ONLY FOR CONVENIENCE. ⚠️⚠️⚠️
 * The sources of truth are at `build/build/src/project/gui2.rs` and
 * `build/build/src/ide/web/fonts.rs`. */

import * as fsSync from 'node:fs'
import * as fs from 'node:fs/promises'
import * as http from 'node:http'
import * as https from 'node:https'
import * as process from 'node:process'
import tar from 'tar'
import bz2 from 'unbzip2-stream'

if (process.env.CI === '1') process.exit(0)

const WARNING_MESSAGE =
  '⚠️⚠️⚠️ Please use the buildscript (`./run`) to download fonts instead. ⚠️⚠️⚠️'
let warningMessageAlreadyShown = false
let exitCode = 0

const ENSO_FONT_URL = 'https://github.com/enso-org/font/releases/download/1.0/enso-font-1.0.tar.gz'
const MPLUS1_FONT_URL =
  'https://github.com/coz-m/MPLUS_FONTS/raw/71d438c798d063cc6fdae8d2864bc48f2d3d06ad/fonts/ttf/MPLUS1%5Bwght%5D.ttf'
const DEJAVU_SANS_MONO_FONT_URL =
  'https://sourceforge.net/projects/dejavu/files/dejavu/2.37/dejavu-fonts-ttf-2.37.tar.bz2'

/** @param {string | https.RequestOptions | URL} options
 * @param {((res: import('node:http').IncomingMessage) => void) | undefined} [callback] */
function get(options, callback) {
  const protocol =
    typeof options === 'string' ? new URL(options).protocol : options.protocol ?? 'https:'
  /** @type {{ get: typeof http['get'] }} */
  let httpModule = https
  switch (protocol) {
    case 'http:': {
      httpModule = http
      break
    }
  }
  return httpModule.get(options, (response) => {
    const location = response.headers.location
    if (location) {
      get(
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

/** @param {unknown} error */
function errorCode(error) {
  return typeof error === 'object' &&
    error != null &&
    'code' in error &&
    typeof error.code === 'string'
    ? error.code
    : undefined
}

/** @param {unknown} error */
function isFileNotFoundError(error) {
  return errorCode(error) === 'ENOENT'
}

const ENSO_FONT_VARIANTS = [
  { variant: 'Thin', weight: 100 },
  { variant: 'ExtraLight', weight: 200 },
  { variant: 'Light', weight: 300 },
  { variant: 'Regular', weight: 400 },
  { variant: 'Medium', weight: 500 },
  { variant: 'SemiBold', weight: 600 },
  { variant: 'Bold', weight: 700 },
  { variant: 'ExtraBold', weight: 800 },
  { variant: 'Black', weight: 900 },
].map((variant) => ({ font: 'Enso', ...variant }))

const DEJAVU_FONT_VARIANTS = [
  { variant: 'DejaVuSansMono', weight: 400 },
  { variant: 'DejaVuSansMono-Bold', weight: 700 },
].map((variant) => ({ font: 'DejaVu Sans Mono', ...variant }))

try {
  await fs.access(`./src/assets/font-enso.css`)
  for (const { variant } of ENSO_FONT_VARIANTS) {
    await fs.access(`./public/font-enso/Enso-${variant}.ttf`)
  }
  console.info('Enso font already downloaded, skipping...')
} catch (error) {
  if (!isFileNotFoundError(error)) {
    console.error('Unexpected error occurred when checking for Enso font:')
    console.error(error)
    exitCode = 1
  } else {
    if (!warningMessageAlreadyShown) console.warn(WARNING_MESSAGE)
    warningMessageAlreadyShown = true
    console.info('Downloading Enso font...')
    await fs.rm('./public/font-enso/', { recursive: true, force: true })
    await fs.mkdir('./public/font-enso/', { recursive: true })
    await new Promise((resolve, reject) => {
      get(ENSO_FONT_URL, (response) => {
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
    /** @type {string[]} */
    let css = []
    for (const { font, variant, weight } of ENSO_FONT_VARIANTS) {
      css.push(`\
@font-face {
  font-family: '${font}';
  src: url('/font-enso/Enso-${variant}.ttf');
  font-weight: ${weight};
}
`)
    }
    await fs.writeFile('./src/assets/font-enso.css', css.join('\n'))
  }
}
try {
  await fs.access(`./src/assets/font-mplus1.css`)
  await fs.access(`./public/font-mplus1/MPLUS1[wght].ttf`)
  console.info('M PLUS 1 font already downloaded, skipping...')
} catch (error) {
  if (!isFileNotFoundError(error)) {
    console.error('Unexpected error occurred when checking for M PLUS 1 font:')
    console.error(error)
    exitCode = 1
  } else {
    if (!warningMessageAlreadyShown) console.warn(WARNING_MESSAGE)
    warningMessageAlreadyShown = true
    console.info('Downloading M PLUS 1 font...')
    await fs.rm('./public/font-mplus1/', { recursive: true, force: true })
    await fs.mkdir('./public/font-mplus1/', { recursive: true })
    await new Promise((resolve, reject) => {
      get(MPLUS1_FONT_URL, (response) => {
        response.pipe(fsSync.createWriteStream('./public/font-mplus1/MPLUS1[wght].ttf'))
        response.on('end', resolve)
        response.on('error', reject)
      })
    })
    const css = `\
@font-face {
  font-family: 'M PLUS 1';
  src: url('/font-mplus1/MPLUS1[wght].ttf');
}
`
    await fs.writeFile('./src/assets/font-mplus1.css', css)
  }
}
try {
  await fs.access(`./src/assets/font-dejavu.css`)
  for (const variant of ['', '-Bold']) {
    await fs.access(`./public/font-dejavu/DejaVuSansMono${variant}.ttf`)
  }
  console.info('DejaVu Sans Mono font already downloaded, skipping...')
} catch (error) {
  if (!isFileNotFoundError(error)) {
    console.error('Unexpected error occurred when checking for DejaVu Sans Mono font:')
    console.error(error)
    exitCode = 1
  } else {
    if (!warningMessageAlreadyShown) console.warn(WARNING_MESSAGE)
    warningMessageAlreadyShown = true
    console.info('Downloading DejaVu Sans Mono font...')
    await fs.rm('./public/font-dejavu/', { recursive: true, force: true })
    await fs.mkdir('./public/font-dejavu/', { recursive: true })
    await new Promise((resolve, reject) => {
      get(DEJAVU_SANS_MONO_FONT_URL, (response) => {
        response.pipe(bz2()).pipe(
          tar.extract({
            cwd: './public/font-dejavu/',
            strip: 2,
            filter(path) {
              return /[\\/]DejaVuSansMono/.test(path) && !/Oblique[.]ttf$/.test(path)
            },
          }),
        )
        response.on('end', resolve)
        response.on('error', reject)
      })
    })
    /** @type {string[]} */
    let css = []
    for (const { font, variant, weight } of DEJAVU_FONT_VARIANTS) {
      css.push(`\
@font-face {
  font-family: '${font}';
  src: url('/font-dejavu/${variant}.ttf');
  font-weight: ${weight};
}
`)
    }
    await fs.writeFile('./src/assets/font-dejavu.css', css.join('\n'))
  }
}
console.info('Done.')
if (exitCode !== 0) process.exit(exitCode)
