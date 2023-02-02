import * as semver from 'semver'
// @ts-ignore
import * as app from 'ensogl_app'
import buildCfg from '../../../build.json'

const config = app.config
const Option = config.Option
const Config = config.Config
export { Option, Config }

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
        groups: {
            startup: new config.Group({
                options: {
                    // @ts-ignore
                    project: new config.Option({
                        default: '',
                        description: 'Project name to open on startup.',
                    }),
                    // @ts-ignore
                    platform: new config.Option({
                        default: '',
                        description:
                            'The host platform the app is running on. This is used to adjust some UI elements. ' +
                            'For example, on macOS, the window close buttons are integrated to the top app panel.',
                        primary: false,
                    }),
                    // @ts-ignore
                    isInCloud: new config.Option({
                        default: false,
                        description: 'Information if the app is running in the cloud.',
                        primary: false,
                    }),
                },
            }),

            engine: new config.Group({
                options: {
                    // @ts-ignore
                    projectManager: new config.Option({
                        default: '',
                        description: 'An address of the Project Manager service.',
                    }),
                    // @ts-ignore
                    languageServerRpc: new config.Option({
                        default: '',
                        description:
                            'An address of the Language Server RPC endpoint. This argument should be provided ' +
                            'together with `languageServerData` ,`namespace`, and `project` options. They make ' +
                            'Enso connect directly to the already spawned Language Server of some project.',
                    }),
                    // @ts-ignore
                    languageServerData: new config.Option({
                        default: '',
                        description:
                            'An address of the Language Server Data endpoint. This argument should be provided ' +
                            'together with `languageServerData` ,`namespace`, and `project` options. They make ' +
                            'Enso connect directly to the already spawned Language Server of some project.',
                    }),
                    // @ts-ignore
                    namespace: new config.Option({
                        default: '',
                        description:
                            'Informs Enso about namespace of the opened project. May be used when connecting to ' +
                            'existing Language Server process. Defaults to "local".',
                    }),
                    // @ts-ignore
                    applicationConfigUrl: new config.Option({
                        default:
                            'https://raw.githubusercontent.com/enso-org/ide/develop/config.json',
                        description:
                            'The application config URL. Used to check for available updates.',
                    }),
                    // @ts-ignore
                    skipMinVersionCheck: new config.Option({
                        default: Version.isDev(),
                        description:
                            'Controls whether the minimum engine version check should be performed. It is set to ' +
                            '`true` in local builds.',
                    }),
                    // @ts-ignore
                    preferredEngineVersion: new config.Option({
                        default: Version.ide,
                        description: `The preferred engine version.`,
                    }),
                },
            }),

            style: new config.Group({
                options: {
                    // @ts-ignore
                    frame: new config.Option({
                        default: false,
                        description:
                            'Controls whether a window frame should be visible. Works in native app only.',
                    }),
                    // @ts-ignore
                    darkTheme: new config.Option({
                        default: false,
                        description:
                            'Controls whether the dark theme should be used. Please note that the dark theme is ' +
                            'not fully implemented yet.',
                    }),
                    // @ts-ignore
                    nodeLabels: new config.Option({
                        default: true,
                        description: `Controls whether node labels should be visible.`,
                    }),
                    // @ts-ignore
                    enableNewComponentBrowser: new config.Option({
                        default: true,
                        description:
                            'Controls whether the new component browser should be enabled.',
                    }),
                },
            }),

            runtimeMetrics: new config.Group({
                options: {
                    // @ts-ignore
                    dataGathering: new config.Option({
                        default: true,
                        description: 'Controls whether anonymous data gathering should be enabled.',
                    }),
                    // @ts-ignore
                    authenticationEnabled: new config.Option({
                        default: true,
                        description: 'Controls whether user authentication is enabled.',
                    }),
                    // @ts-ignore
                    email: new config.Option({
                        default: '',
                        description: 'The user email, if any.',
                    }),
                },
            }),

            debug: new config.Group({
                options: {
                    // @ts-ignore
                    testWorkflow: new config.Option({
                        default: '',
                        description:
                            'When profiling the application (e.g. with the `./run profile` command), this ' +
                            'argument chooses what is profiled.',
                    }),
                    // @ts-ignore
                    debug: new config.Option({
                        default: Version.isDev(),
                        description:
                            'Controls whether the application should be run in the debug mode. In this mode all ' +
                            'logs are printed to the console. Otherwise, the logs are hidden unless explicitly ' +
                            'shown by calling `showLogs`. Moreover, additional logs from libraries are printed ' +
                            'in this mode. The debug mode is set to `true` by default in local builds.',
                    }),
                    // @ts-ignore
                    emitUserTimingMeasurements: new config.Option({
                        default: false,
                        description:
                            'When enabled, profiling measurements will be continually submitted to the User ' +
                            'Timing Web API so that they can be viewed with standard developer tools. Note that ' +
                            'this mode has a significant performance impact.',
                    }),
                },
            }),
        },
    })
)
