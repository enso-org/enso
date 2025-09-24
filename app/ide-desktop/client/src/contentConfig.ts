/** @file Configuration options of the application content (the web part). */

import * as semver from 'semver'

import * as linkedDist from 'enso-runner/src/runner'
import BUILD_INFO from '../buildInfo'

export const Option = linkedDist.config.Option
export const Group = linkedDist.config.Group
/** A configuration option. */
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

const options = {}
const groups = {
  startup: new Group({
    description: undefined,
    groups: {},
    options: {
      project: new Option({
        description:
          'The name of the project to open at startup. If the project does not exist, it will be created.',
        value: '',
      }),
      displayedProjectName: new Option({
        description: 'The name of the project to be displayed to the user.',
        value: '',
        primary: false,
      }),
    },
  }),
  engine: new Group({
    description: 'Options that control the Enso Engine, the data processing backend.',
    groups: {},
    options: {
      projectManagerUrl: new Option({
        description: 'The address of the Project Manager service.',
        value: '',
        primary: false,
      }),
      ydocUrl: new Option({
        description: 'The address of the Ydoc Server endpoint.',
        value: '',
        primary: false,
      }),
    },
  }),
  authentication: new Group({
    description: 'Options to manage application authentication properties.',
    groups: {},
    options: {
      enabled: new Option({
        description:
          'Determines whether user authentication is enabled. This option is always true when executed in the cloud.',
        value: true,
      }),
      email: new Option({
        description: 'The user email, if the user is logged in.',
        value: '',
        primary: false,
      }),
    },
  }),
}

export const OPTIONS = new Group({ description: undefined, options, groups })
