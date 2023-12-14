import type { options } from '@/util/appOptions'
import { fetchTimeout } from '@/util/fetchTimeout'
import { Comparator, SemVer } from 'semver'
// @ts-ignore
import BUILD_INFO from '../../../../build.json' assert { type: 'json' }

/** Time in seconds after which a `fetchTimeout` ends. */
const FETCH_TIMEOUT = 300

/** Development version. */
export const dev = new SemVer('0.0.0')

export const devPrerelease = 'dev'

/** The version of the GUI. */
export const ide = new SemVer(BUILD_INFO.version, { loose: true })

/** Whether this is a development version. */
export function isDev() {
  const clientVersion = ide
  const releaseDev = clientVersion.compareMain(dev) === 0
  const prereleaseDev = clientVersion.prerelease.toString().includes(devPrerelease)
  return releaseDev || prereleaseDev
}

/** Return `true` if the current application version is still supported and `false` otherwise.
 *
 * Function downloads the application config containing the minimum supported version from GitHub
 * and compares it with the version of the `client` js package. When the function is unable to
 * download the application config, or one of the compared versions does not match the semver
 * scheme, it returns `true`. */
export async function checkMinSupportedVersion(config: typeof options) {
  let supported = false
  if (config.groups.engine.options.skipMinVersionCheck.value) {
    supported = true
  } else {
    try {
      const appConfig = await fetchTimeout(
        config.groups.engine.options.configUrl.value,
        FETCH_TIMEOUT,
      )
      if (
        typeof appConfig === 'object' &&
        appConfig != null &&
        'minimumSupportedVersion' in appConfig
      ) {
        const minSupportedVersion = appConfig.minimumSupportedVersion
        if (typeof minSupportedVersion !== 'string') {
          console.error('The minimum supported version is not a string.')
        } else {
          const comparator = new Comparator(`>=${minSupportedVersion}`)
          supported = comparator.test(ide)
        }
      } else {
        console.error('The application config is not an object.')
      }
    } catch (e) {
      console.error('Minimum version check failed.', e)
      supported = true
    }
  }
  return supported
}

/** Display information that the current app version is deprecated. */
export function displayDeprecatedVersionDialog() {
  const root = document.getElementById('root')
  if (root == null) {
    console.error('Cannot find the root DOM element.')
  } else {
    const versionCheckDiv = root.appendChild(document.createElement('div'))
    versionCheckDiv.style.textAlign = 'center'
    versionCheckDiv.style.fontFamily = 'sans-serif'
    versionCheckDiv.style.color = '#454545'
    versionCheckDiv.style.margin = '24px auto'
    versionCheckDiv.style.display = 'none'
    versionCheckDiv.style.display = 'block'
    versionCheckDiv.appendChild(
      document.createTextNode('This version is no longer supported. Please download a new one.'),
    )
  }
}
