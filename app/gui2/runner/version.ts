import { SemVer } from 'semver'

// @ts-ignore
import BUILD_INFO from '../../../build.json' assert { type: 'json' }

export const version = {
  /// Development version.
  dev: new SemVer('0.0.0'),
  devPrerelease: 'dev',

  /// Version of the `client` js package.
  ide: new SemVer(BUILD_INFO.version, { loose: true }),

  /** Returns whether this is a development version. */
  isDev(): boolean {
    const clientVersion = version.ide
    const releaseDev = clientVersion.compareMain(version.dev) === 0
    const prereleaseDev = clientVersion.prerelease.toString().includes(version.devPrerelease)
    return releaseDev || prereleaseDev
  },
}
