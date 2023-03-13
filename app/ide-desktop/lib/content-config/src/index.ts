/** @file Configuration options of the application content (the web part). */

import * as semver from 'semver'

import * as linkedDist from '../../../../../target/ensogl-pack/linked-dist/index'
import buildCfg from '../../../build.json'

// Aliases with the same name as the original.
// eslint-disable-next-line @typescript-eslint/naming-convention
export const Option = linkedDist.config.Option
// eslint-disable-next-line @typescript-eslint/naming-convention
export const Group = linkedDist.config.Group
export const LOGGER = linkedDist.log.logger
// Declaring type with the same name as a variable.
// eslint-disable-next-line @typescript-eslint/no-redeclare
export type Option<T> = linkedDist.config.Option<T>

// ===============
// === Version ===
// ===============

export class Version {
    /// Development version.
    static dev = new semver.SemVer('0.0.0')
    static devPrerelease = 'dev'

    /// Version of the `client` js package.
    static ide = new semver.SemVer(buildCfg.version, { loose: true })

    static isDev(): boolean {
        const clientVersion = Version.ide
        const releaseDev = clientVersion.compareMain(Version.dev) === 0
        const prereleaseDev = clientVersion.prerelease.toString().includes(Version.devPrerelease)
        return releaseDev || prereleaseDev
    }
}

// ===============
// === Options ===
// ===============

import * as jsonCfg from './config.json'
export const OPTIONS = linkedDist.config.options.merge(linkedDist.config.objectToGroup(jsonCfg, { Version }))
