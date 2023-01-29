import * as semver from 'semver'
// @ts-ignore
import * as app from 'ensogl_app'
import buildCfg from '../../../build.json'

const config = app.config
const Param = config.Param
export { Param }

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

// ==============
// === Config ===
// ==============

export class Config extends config.Params {
    // @ts-ignore
    project: config.Param<string | null> = new config.Param({
        type: 'string',
        default: null,
        description: 'Project name to open on startup.',
    })
    // @ts-ignore
    projectManager: config.Param<string | null> = new config.Param({
        type: 'string',
        default: null,
        description: 'An address of the Project Manager service.',
    })
    // @ts-ignore
    languageServerRpc: config.Param<string | null> = new config.Param({
        type: 'string',
        default: null,
        description:
            'An address of the Language Server RPC endpoint. This argument should be provided ' +
            'together with `languageServerData` ,`namespace`, and `project` options. They make ' +
            'Enso connect directly to the already spawned Language Server of some project.',
    })
    // @ts-ignore
    languageServerData: config.Param<string | null> = new config.Param({
        type: 'string',
        default: null,
        description:
            'An address of the Language Server Data endpoint. This argument should be provided ' +
            'together with `languageServerData` ,`namespace`, and `project` options. They make ' +
            'Enso connect directly to the already spawned Language Server of some project.',
    })
    // @ts-ignore
    namespace: config.Param<string | null> = new config.Param({
        type: 'string',
        default: null,
        description:
            'Informs Enso about namespace of the opened project. May be used when connecting to ' +
            'existing Language Server process. Defaults to "local".',
    })
    // @ts-ignore
    platform: config.Param<string | null> = new config.Param({
        type: 'string',
        default: null,
        description:
            'The host platform the app is running on. This is used to adjust some UI elements. ' +
            'For example, on macOS, the window close buttons are integrated to the top app panel.',
    })
    // @ts-ignore
    frame: config.Param<boolean> = new config.Param({
        type: 'boolean',
        default: false,
        description: 'Controls whether a window frame should be visible. Works in native app only.',
    })
    // @ts-ignore
    darkTheme: config.Param<boolean> = new config.Param({
        type: 'boolean',
        default: false,
        description:
            'Controls whether the dark theme should be used. Please note that the dark theme is ' +
            'not fully implemented yet.',
    })
    // @ts-ignore
    nodeLabels: config.Param<boolean> = new config.Param({
        type: 'boolean',
        default: true,
        description: `Controls whether node labels should be visible.`,
    })
    // @ts-ignore
    dataGathering: config.Param<boolean> = new config.Param({
        type: 'boolean',
        default: true,
        description: 'Controls whether anonymous data gathering should be enabled.',
    })
    // @ts-ignore
    isInCloud: config.Param<boolean> = new config.Param({
        type: 'boolean',
        default: false,
        description: 'Information if the app is running in the cloud.',
    })
    // @ts-ignore
    authenticationEnabled: config.Param<boolean> = new config.Param({
        type: 'boolean',
        default: true,
        description: 'Controls whether user authentication is enabled.',
    })
    // @ts-ignore
    email: config.Param<string | null> = new config.Param({
        type: 'string',
        default: null,
        description: 'The user email, if any.',
    })
    // @ts-ignore
    applicationConfigUrl: config.Param<string> = new config.Param({
        type: 'string',
        default: 'https://raw.githubusercontent.com/enso-org/ide/develop/config.json',
        description: 'The application config URL. Used to check for available updates.',
    })
    // @ts-ignore
    testWorkflow: config.Param<string | null> = new config.Param({
        type: 'string',
        default: null,
        description:
            'When profiling the application (e.g. with the `./run profile` command), this ' +
            'argument chooses what is profiled.',
    })
    // @ts-ignore
    skipMinVersionCheck: config.Param<boolean> = new config.Param({
        type: 'boolean',
        default: Version.isDev(),
        description:
            'Controls whether the minimum engine version check should be performed. It is set to ' +
            '`true` in local builds.',
    })
    // @ts-ignore
    debug: config.Param<boolean> = new config.Param({
        type: 'boolean',
        default: Version.isDev(),
        description:
            'Controls whether the application should be run in the debug mode. In this mode all ' +
            'logs are printed to the console. Otherwise, the logs are hidden unless explicitly ' +
            'shown by calling `showLogs`. Moreover, additional logs from libraries are printed ' +
            'in this mode. The debug mode is set to `true` by default in local builds.',
    })
    // @ts-ignore
    preferredEngineVersion: config.Param<semver.SemVer> = new config.Param({
        type: 'string',
        default: Version.ide,
        description: `The preferred engine version.`,
    })
    // @ts-ignore
    enableNewComponentBrowser: config.Param<boolean> = new config.Param({
        type: 'boolean',
        default: true,
        description: 'Controls whether the new component browser should be enabled.',
    })
    // @ts-ignore
    emitUserTimingMeasurements: config.Param<boolean> = new config.Param({
        type: 'boolean',
        default: false,
        description:
            'When enabled, profiling measurements will be continually submitted to the User ' +
            'Timing Web API so that they can be viewed with standard developer tools. Note that ' +
            'this mode has a significant performance impact.',
    })
}
