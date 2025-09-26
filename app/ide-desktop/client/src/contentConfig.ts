/** @file Configuration options of the application content (the web part). */

import * as semver from 'semver'

import BUILD_INFO from '../buildInfo'

// ===============
// === Version ===
// ===============

export const VERSION = {
  /// Development version.
  dev: new semver.SemVer('0.0.0'),
  devPrerelease: 'dev',

  /// Version of the `client` js package.
  ide: new semver.SemVer(BUILD_INFO.version, { loose: true }),

  /** Returns whether this is a development version. */
  isDev(): boolean {
    const clientVersion = VERSION.ide
    const releaseDev = clientVersion.compareMain(VERSION.dev) === 0
    const prereleaseDev = clientVersion.prerelease.toString().includes(VERSION.devPrerelease)
    return releaseDev || prereleaseDev
  },
}
