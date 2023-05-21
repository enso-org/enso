/** @file Configuration options of the application content (the web part). */

import * as semver from 'semver'

import * as linkedDist from '../../../../../target/ensogl-pack/linked-dist/index'
import BUILD_INFO from '../../../build.json' assert { type: 'json' }

// Aliases with the same name as the original.
// eslint-disable-next-line no-restricted-syntax
export const Option = linkedDist.config.Option
// eslint-disable-next-line no-restricted-syntax
export const Group = linkedDist.config.Group
export const logger = linkedDist.log.logger
/** A configuration option. */
// This type has the same name as a variable.
// eslint-disable-next-line @typescript-eslint/no-redeclare
export type Option<T> = linkedDist.config.Option<T>

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

// ===============
// === Options ===
// ===============

import CONFIG from './config.json' assert { type: 'json' }

export const OPTIONS = linkedDist.config.options.merge(
    // `valueEval` in `config.json` uses PascalCase scope variables for some reason.
    // eslint-disable-next-line @typescript-eslint/naming-convention
    linkedDist.config.objectToGroup(CONFIG, { Version: VERSION })
)
