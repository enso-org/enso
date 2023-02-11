import * as content from '../../content/src/config'
import * as paths from './paths'

// ==================
// === WindowSize ===
// ==================

/** Window size (width and height). */
export class WindowSize {
    static separator = 'x'
    constructor(public width: number, public height: number) {}

    /** Constructor of the default window size. */
    static default(): WindowSize {
        return new WindowSize(1380, 900)
    }

    /** Parses the input text in form of `<width>x<height>`. */
    static parse(arg: string): Error | WindowSize {
        const size = arg.split(WindowSize.separator)
        const widthStr = size[0]
        const heightStr = size[1]
        const width = widthStr ? parseInt(widthStr) : NaN
        const height = heightStr ? parseInt(heightStr) : NaN
        if (isNaN(width) || isNaN(height)) {
            return new Error(`Incorrect window size provided '${arg}'.`)
        } else {
            return new WindowSize(width, height)
        }
    }

    /** Returns window size in a form of `<width>x<height>`. */
    pretty(): string {
        return `${this.width}${WindowSize.separator}${this.height}`
    }
}

// ==============
// === Config ===
// ==============

export const my_args = content.options.merge(
    new content.Group({
        options: {
            window: new content.Option({
                default: true,
                description:
                    'Show the window. If set to false, only the server will be run. You can use another ' +
                    'client or a browser to connect to it.',
            }),
            server: new content.Option({
                default: true,
                description:
                    'Run the server. If set to false, you can connect to an existing server on the ' +
                    'provided `port`.',
            }),
            showElectronOptions: new content.Option({
                default: false,
                description:
                    'Show Electron options in the help. Should be used together with `-help`.',
            }),
            info: new content.Option({
                default: false,
                description: `Print the system debug info.`,
            }),
            version: new content.Option({
                default: false,
                description: `Print the version.`,
            }),
            help: new content.Option({
                default: false,
                description:
                    'Show the common configuration options help page. ' +
                    'To see all options, use `-full-help`.',
            }),
            fullHelp: new content.Option({
                default: false,
                description: 'Show all the configuration options help page.',
            }),
        },
        groups: {
            window: new content.Group({
                options: {
                    size: new content.Option({
                        default: WindowSize.default().pretty(),
                        description: `The initial window size.`,
                    }),
                    frame: new content.Option({
                        default: process.platform !== 'darwin',
                        defaultDescription: 'false on MacOS, true otherwise',
                        description: 'Draw window frame.',
                    }),
                    vibrancy: new content.Option({
                        default: false,
                        description: 'Use the vibrancy effect.',
                    }),
                },
            }),
            server: new content.Group({
                options: {
                    port: new content.Option({
                        default: 8080,
                        description: `Port to use. In case the port is unavailable, next free port will be found.`,
                    }),
                },
            }),

            performance: new content.Group({
                options: {
                    backgroundThrottling: new content.Option({
                        default: false,
                        description: 'Throttle animations when run in background.',
                    }),
                    loadProfile: new content.Option({
                        // FIXME
                        default: [],
                        description:
                            'Load a performance profile. For use with developer tools such as the `profiling-run-graph` entry point.',
                    }),
                    saveProfile: new content.Option({
                        default: '',
                        description: 'Record a performance profile and write to a file.',
                    }),
                    workflow: new content.Option({
                        default: '',
                        description:
                            'Specify a workflow for profiling. Must be used with -entry-point=profile.',
                    }),
                },
            }),

            engine: new content.Group({
                options: {
                    backend: new content.Option({
                        default: true,
                        description: 'Start the backend process.',
                    }),
                    projectManagerPath: new content.Option({
                        default: paths.projectManager,
                        description:
                            'Set the path of a local project manager to use for running projects',
                    }),
                },
            }),

            debug: new content.Group({
                options: {
                    verbose: new content.Option({
                        default: false,
                        description: `Increase logs verbosity. Affects both IDE and the backend.`,
                    }),
                    dev: new content.Option({
                        default: false,
                        description: 'Run the application in development mode.',
                    }),
                    devtron: new content.Option({
                        default: false,
                        description: 'Install the Devtron Developer Tools extension.',
                    }),
                },
            }),
            electron: new content.Group({
                options: {
                    // === Electron Options ===
                    // https://www.electronjs.org/docs/latest/api/command-line-switches

                    authServerWhitelist: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            'A comma-separated list of servers for which integrated authentication is ' +
                            'enabled.',
                    }),
                    authNegotiateDelegateWhitelist: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            'A comma-separated list of servers for which delegation of user credentials is ' +
                            "required. Without '*' prefix the URL has to match exactly.",
                    }),
                    disableNtlmV2: new content.Option({
                        hidden: true,
                        default: false,
                        description: 'Disables NTLM v2 for posix platforms, no effect elsewhere.',
                    }),
                    disableHttpCache: new content.Option({
                        hidden: true,
                        default: false,
                        description: 'Disables the disk cache for HTTP requests.',
                    }),
                    disableHttp2: new content.Option({
                        hidden: true,
                        default: false,
                        description: 'Disable HTTP/2 and SPDY/3.1 protocols.',
                    }),
                    disableRendererBackgrounding: new content.Option({
                        hidden: true,
                        default: false,
                        description:
                            "Prevents Chromium from lowering the priority of invisible pages' renderer " +
                            'processes.',
                    }),
                    diskCacheSize: new content.Option({
                        hidden: true,
                        default: 0,
                        description:
                            'Forces the maximum disk space to be used by the disk cache, in bytes.',
                    }),
                    enableLogging: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            "Prints Chromium's logging to stderr (or a log file, if provided as argument).",
                    }),
                    forceFieldtrials: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            'Field trials to be forcefully enabled or disabled. For example, ' +
                            "'WebRTC-Audio-Red-For-Opus/Enabled/'.",
                    }),
                    hostRules: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            'A comma-separated list of rules that control how hostnames are mapped. For ' +
                            "example, 'MAP * 127.0.0.1'.",
                    }),
                    hostResolverRules: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            "Like '--host-rules' but these rules only apply to the host resolver.",
                    }),
                    ignoreCertificateErrors: new content.Option({
                        hidden: true,
                        default: false,
                        description: 'Ignores certificate related errors.',
                    }),
                    ignoreConnectionsLimit: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            "Ignore the connections limit for domains list separated by ','.",
                    }),
                    jsFlags: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            'Specifies the flags passed to the Node.js engine. For example, ' +
                            '\'-electron-js-flags="--harmony_proxies --harmony_collections"\'.',
                    }),
                    lang: new content.Option({
                        hidden: true,
                        default: '',
                        description: 'Set a custom locale.',
                    }),
                    logFile: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            "If '-electron-enable-logging' is specified, logs will be written to the given path. " +
                            'The parent directory must exist.',
                    }),
                    logNetLog: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            'Enables net log events to be saved and writes them to the provided path.',
                    }),
                    logLevel: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            "Sets the verbosity of logging when used together with '-electron-enable-logging'. " +
                            "The argument should be one of Chrome's LogSeverities.",
                    }),
                    noProxyServer: new content.Option({
                        hidden: true,
                        default: false,
                        description:
                            "Don't use a proxy server and always make direct connections. Overrides " +
                            'any other proxy server flags that are passed.',
                    }),
                    noSandbox: new content.Option({
                        hidden: true,
                        default: false,
                        description:
                            'Disables the Chromium sandbox. Forces renderer process and Chromium helper ' +
                            'processes to run un-sandboxed. Should only be used for testing.',
                    }),
                    proxyBypassList: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            'Instructs Electron to bypass the proxy server for the given ' +
                            'semi-colon-separated list of hosts. This flag has an effect only if used in tandem ' +
                            "with '--proxy-server'. For example, " +
                            '\'--proxy-bypass-list "<local>;*.google.com;*foo.com;1.2.3.4:5678"\'.',
                    }),
                    proxyPacUrl: new content.Option({
                        hidden: true,
                        default: '',
                        description: 'Uses the PAC script at the specified url.',
                    }),
                    proxyServer: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            "Use a specified proxy server ('address:port'), which overrides the system " +
                            'setting. This switch only affects requests with HTTP protocol, including HTTPS and ' +
                            'WebSocket requests. It is also noteworthy that not all proxy servers support HTTPS ' +
                            'and WebSocket requests. The proxy URL does not support username and password ' +
                            'authentication per ' +
                            '[Chromium issue](https://bugs.chromium.org/p/chromium/issues/detail?id=615947).',
                    }),
                    remoteDebuggingPort: new content.Option({
                        hidden: true,
                        default: '',
                        description: 'Enables remote debugging over HTTP on the specified port.',
                    }),
                    v: new content.Option({
                        hidden: true,
                        default: 0,
                        description:
                            'Gives the default maximal active V-logging level; 0 is the default. Normally ' +
                            'positive values are used for V-logging levels. This switch only works when ' +
                            "'-electron-enable-logging' is also passed.",
                    }),
                    vmodule: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            'Gives the per-module maximal V-logging levels to override the value given by ' +
                            "'-electron-v'. E.g. 'my_module=2,foo*=3' would change the logging level for all code in " +
                            "source files 'my_module.*' and 'foo*.*'. Any pattern containing a forward or " +
                            'backward slash will be tested against the whole pathname and not only the module. ' +
                            "This switch only works when '-electron-enable-logging' is also passed.",
                    }),
                    force_high_performance_gpu: new content.Option({
                        hidden: true,
                        default: false,
                        description:
                            'Force using discrete GPU when there are multiple GPUs available.',
                    }),
                    force_low_power_gpu: new content.Option({
                        hidden: true,
                        default: false,
                        description:
                            'Force using integrated GPU when there are multiple GPUs available.',
                    }),
                },
            }),
        },
    })
)
my_args.groups.startup.options.platform.default = process.platform
my_args.groups.startup.options.platform.value = process.platform

export type Args = typeof my_args
