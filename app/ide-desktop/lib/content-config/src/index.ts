/** @file Configuration options of the application content (the web part). */

import * as semver from 'semver'
import { log, config, App } from '../../../../../target/ensogl-pack/linked-dist/index'
import buildCfg from '../../../build.json'

const Option = config.Option
const Group = config.Group
const logger = log.logger
export { Option, Group, logger }
export type Option<T> = config.Option<T>

// ===============
// === Version ===
// ===============s

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
export const options = config.options.merge(config.objectToGroup(jsonCfg, { Version }))
