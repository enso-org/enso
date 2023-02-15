/** @file Configuration options of the application content (the web part). */

import * as semver from 'semver'
import { log, config, App } from '../../../../../target/ensogl-pack/dist/index'
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

export const options = config.options.merge(
    new config.Group({
        options: {
            authentication: new config.Option({
                default: true,
                description:
                    'Controls whether user authentication is enabled. This option is ignored if ' +
                    'the app is run in the cloud.',
                primary: false,
            }),

            dataCollection: new config.Option({
                default: true,
                description: 'Controls whether anonymous usage data should be collected.',
                primary: false,
            }),
        },
        groups: {
            dataCollection: new config.Group({
                description: 'Controls whether anonymous usage data should be collected.',
                options: {
                    usageStatistics: new config.Option({
                        default: '',
                        description:
                            'Controls whether anonymous usage statistics should be collected.',
                        primary: false,
                    }),
                    crashReports: new config.Option({
                        default: '',
                        description:
                            'Controls whether anonymous crash reports should be collected.',
                        primary: false,
                    }),
                },
            }),

            startup: new config.Group({
                options: {
                    project: new config.Option({
                        default: '',
                        description:
                            'Project name to open on startup. If the project does not exist, it will be created.',
                    }),
                    platform: new config.Option({
                        default: 'web',
                        defaultDescription: `'web' if run in the browser, operating system name otherwise`,
                        description:
                            'The host platform the app is running on. This is used to adjust some UI elements. ' +
                            'For example, on macOS, the window close buttons are integrated to the top app panel.',
                        primary: false,
                    }),
                },
            }),

            engine: new config.Group({
                description: 'Options controlling the Enso Engine, the data processing backend.',
                options: {
                    projectManagerUrl: new config.Option({
                        default: '',
                        description: 'An address of the Project Manager service.',
                        primary: false,
                    }),
                    rpcUrl: new config.Option({
                        default: '',
                        description:
                            'An address of the Language Server RPC endpoint. This argument should be provided ' +
                            'together with `dataUrl` ,`namespace`, and `project` options. They make ' +
                            'Enso connect directly to the already spawned Language Server of some project.',
                        primary: false,
                    }),
                    dataUrl: new config.Option({
                        default: '',
                        description:
                            'An address of the Language Server Data endpoint. This argument should be provided ' +
                            'together with `dataUrl` ,`namespace`, and `project` options. They make ' +
                            'Enso connect directly to the already spawned Language Server of some project.',
                        primary: false,
                    }),
                    namespace: new config.Option({
                        default: 'local',
                        description:
                            'Namespace of the opened project. May be used when connecting to ' +
                            'existing Language Server process.',
                        primary: false,
                    }),
                    configUrl: new config.Option({
                        default:
                            'https://raw.githubusercontent.com/enso-org/ide/develop/config.json',
                        description:
                            'The application config URL. Used to check for available updates.',
                        primary: false,
                    }),
                    skipMinVersionCheck: new config.Option({
                        default: Version.isDev(),
                        defaultDescription: 'true in local builds, false otherwise.',
                        description:
                            'Controls whether the minimum engine version check should be performed.',
                        primary: false,
                    }),
                    preferredVersion: new config.Option({
                        default: String(Version.ide),
                        description: `The preferred engine version.`,
                    }),
                },
            }),

            style: new config.Group({
                description: `The available visual and tactile configurations of the application.`,
                options: {
                    nodeLabels: new config.Option({
                        default: true,
                        description: `Controls whether node labels should be visible.`,
                    }),
                },
            }),

            featurePreview: new config.Group({
                description:
                    'Options allowing enabling experimental features that are not stable yet.',
                options: {
                    newComponentBrowser: new config.Option({
                        default: true,
                        description:
                            'Controls whether the new component browser should be enabled.',
                    }),
                },
            }),

            authentication: new config.Group({
                description:
                    'Options allowing controlling the application authentication properties.',
                options: {
                    email: new config.Option({
                        default: '',
                        description: 'The user email, if any.',
                        primary: false,
                    }),
                },
            }),

            profile: new config.Group({
                description: 'Options allowing diagnosing the application performance problems.',
                options: {
                    testWorkflow: new config.Option({
                        default: '',
                        description:
                            `When profiling the application with the './run profile' command, ` +
                            `this argument chooses what is profiled.`,
                        primary: false,
                    }),
                    emitUserTimingMeasurements: new config.Option({
                        default: false,
                        description:
                            'When enabled, profiling measurements will be continually submitted ' +
                            'to the User Timing Web API so that they can be viewed with standard ' +
                            'developer tools. Note that this mode has a significant performance ' +
                            'impact.',
                        primary: false,
                    }),
                },
            }),
        },
    })
)
