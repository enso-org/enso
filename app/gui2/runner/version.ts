import { SemVer } from 'semver'
// @ts-ignore
import BUILD_INFO from '../../../build.json' assert { type: 'json' }

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
