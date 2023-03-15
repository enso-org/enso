/** @file Configuration of the application. It extends the web application configuration with
 * Electron-specific options. */

import chalk from 'chalk'

import * as contentConfig from 'enso-content-config'

import * as naming from 'naming'
import * as paths from 'paths'

// =================
// === Constants ===
// =================

export const HELP_EXTENDED_NAME = 'helpExtended'
export const HELP_EXTENDED_OPTION_NAME = naming.camelToKebabCase(HELP_EXTENDED_NAME)
const DEFAULT_WIDTH = 1380
const DEFAULT_HEIGHT = 900
const DEFAULT_PORT = 8080

// ==================
// === WindowSize ===
// ==================

/** Window size (width and height). */
export class WindowSize {
    static separator = 'x'
    /** Constructs a new {@link WindowSize}. */
    constructor(public width: number, public height: number) {}

    /** Constructor of the default window size. */
    static default(): WindowSize {
        return new WindowSize(DEFAULT_WIDTH, DEFAULT_HEIGHT)
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

export const CONFIG = contentConfig.OPTIONS.merge(
    new contentConfig.Group({
        options: {
            window: new contentConfig.Option({
                passToWebApplication: false,
                value: true,
                description:
                    `Display the window. When set to false, only the server runs. An ` +
                    `alternative client or browser can connect to it.`,
            }),
            server: new contentConfig.Option({
                passToWebApplication: false,
                value: true,
                description:
                    `Run the server. When set to false, connect to an existing server on the ` +
                    `provided port.`,
            }),
            version: new contentConfig.Option({
                passToWebApplication: false,
                value: false,
                description: `Print the version.`,
            }),
            help: new contentConfig.Option({
                passToWebApplication: false,
                value: false,
                description:
                    `Display the common configuration options help page. Use ` +
                    `'${HELP_EXTENDED_OPTION_NAME}' to see all options.`,
            }),
            [HELP_EXTENDED_NAME]: new contentConfig.Option({
                passToWebApplication: false,
                value: false,
                description:
                    'Show all the configuration options help page, including the less-common ' +
                    'options.',
            }),
            engine: new contentConfig.Option({
                passToWebApplication: false,
                value: true,
                description: 'Start the engine process.',
            }),
        },
        groups: {
            window: new contentConfig.Group({
                options: {
                    size: new contentConfig.Option({
                        passToWebApplication: false,
                        value: WindowSize.default().pretty(),
                        description: `Set the initial window size.`,
                    }),
                    vibrancy: new contentConfig.Option({
                        passToWebApplication: false,
                        value: false,
                        description: `Enable the vibrancy effect.`,
                        primary: false,
                    }),
                    closeToQuit: new contentConfig.Option({
                        passToWebApplication: false,
                        value: process.platform !== 'darwin',
                        defaultDescription: 'false on MacOS, true otherwise',
                        description:
                            `Determine whether the app should quit when the window is closed. ` +
                            `If set to false, the window will be hidden after pressing the close ` +
                            `button. You can bring the window back by pressing the app dock icon.`,
                        primary: false,
                    }),
                },
            }),
            server: new contentConfig.Group({
                description:
                    `The configuration settings for the server utilized in delivering web ` +
                    `application to either Electron or a browser window.`,
                options: {
                    port: new contentConfig.Option({
                        passToWebApplication: false,
                        value: DEFAULT_PORT,
                        description:
                            `Port to use. If the port is unavailable, the next available port is ` +
                            `used.`,
                    }),
                },
            }),

            performance: new contentConfig.Group({
                description: `Performance-related configuration options.`,
                options: {
                    backgroundThrottling: new contentConfig.Option({
                        passToWebApplication: true,
                        value: true,
                        description: 'Throttle animations when run in background.',
                    }),

                    forceHighPerformanceGpu: new contentConfig.Option({
                        passToWebApplication: false,
                        value: true,
                        description:
                            'Force using discrete GPU when there are multiple GPUs available.',
                    }),

                    angleBackend: new contentConfig.Option({
                        passToWebApplication: false,
                        value: 'default',
                        description:
                            `Choose the graphics backend for ANGLE (graphics engine abstraction ` +
                            `layer). The OpenGL backend is soon to be deprecated on Mac, and may ` +
                            `contain driver bugs that are not planned to be fixed. The Metal ` +
                            `backend is still experimental, and may contain bugs that are still ` +
                            `being worked on. However, the 'metal' backend is less performant ` +
                            `than the 'default' one on M1 and M2 Macs, so it is disabled for now.`,
                    }),
                    ignoreGpuBlocklist: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: true,
                        description:
                            `Override the list of blocked GPU hardware, allowing for ` +
                            `GPU acceleration on system configurations that do not inherently ` +
                            `support it. It should be noted that some hardware configurations ` +
                            `may have driver issues that could result in rendering ` +
                            `discrepancies. Despite this, the utilization of GPU acceleration ` +
                            `has the potential to significantly enhance the performance of the ` +
                            `application in our specific use cases. This behavior can be ` +
                            `observed in the following example: ` +
                            `https://groups.google.com/a/chromium.org/g/chromium-dev/c/09NnO6jYT6o.`,
                    }),
                    disableSandbox: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: true,
                        description:
                            `Disable the sandbox feature for all process types that are ` +
                            `typically subjected to sandboxing. This option serves as a ` +
                            `browser-level switch solely for testing purposes. Although Google ` +
                            `discourages the use of this option, it is deemed safe for use in ` +
                            `this particular instance as the browser is exclusively designed to ` +
                            `display Enso, which already has unrestricted access to all files ` +
                            `and system settings on the user's machine. This modification has ` +
                            `been known to result in correct app behavior on certain systems, ` +
                            `as demonstrated in this example: ` +
                            `https://github.com/enso-org/enso/issues/3801.`,
                    }),
                    disableGpuSandbox: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: true,
                        description:
                            `Disable the GPU process sandbox. It should be noted that on ` +
                            `certain hardware configurations, the utilization of GPU sandboxing ` +
                            `may result in WebGL crashes. Despite Google's discouragement of ` +
                            `this option, it is considered safe for use in this specific ` +
                            `instance, as the browser is dedicated solely to the display of ` +
                            `Enso, which has unrestricted access to all files and system ` +
                            `settings on the user's machine. For a detailed explanation of ` +
                            `instances where such crashes may occur, please refer to this ` +
                            `document: https://wiki.archlinux.org/title/chromium.`,
                    }),
                    disableGpuVsync: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: true,
                        description:
                            `Disable the GPU Vertical Synchronization (VSync). This feature ` +
                            `synchronizes the refresh rate and frame rate of the monitor to ` +
                            `ensure optimal picture quality, particularly in gaming scenarios. ` +
                            `However, in applications that heavily rely on a graphical user ` +
                            `interface, the utilization of VSync is not deemed essential. By ` +
                            `disabling this feature, performance may be improved on hardware ` +
                            `configurations with limited capabilities. In addition, disabling ` +
                            `VSync also has the potential to reduce rendering latency. For a ` +
                            `comprehensive understanding of this aspect, please refer to this ` +
                            `thread: https://bugs.chromium.org/p/chromium/issues/detail?id=460919.`,
                    }),
                    disableSmoothScrolling: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: true,
                        description:
                            `Disable smooth scrolling feature. This modification has the ` +
                            `potential to reduce latency experienced with input devices. For ` +
                            `further elaboration, please refer to this thread: ` +
                            `https://news.ycombinator.com/item?id=28782493.`,
                    }),
                    enableNativeGpuMemoryBuffers: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: true,
                        description: `Enable native CPU-mappable GPU memory buffer support on Linux.`,
                    }),
                },
            }),

            profile: new contentConfig.Group({
                options: {
                    loadProfile: new contentConfig.Option<string[]>({
                        passToWebApplication: false,
                        value: [],
                        description:
                            `Load a performance profile. For use with developer tools such as ` +
                            `the 'profiling-run-graph' entry point.`,
                        primary: false,
                    }),
                    saveProfile: new contentConfig.Option({
                        passToWebApplication: false,
                        value: '',
                        description:
                            `Record a performance profile and save it to a file. To view the ` +
                            `results, use the 'profiling-run-graph' entry point, such as ` +
                            `'enso -startup.entry=profiling-run-graph -profile.load-profile=profile.json'.`,
                        primary: false,
                    }),
                },
            }),

            engine: new contentConfig.Group({
                options: {
                    projectManagerPath: new contentConfig.Option({
                        passToWebApplication: false,
                        value: paths.PROJECT_MANAGER_PATH,
                        description:
                            'Set the path of a local project manager executable to use for ' +
                            'running projects.',
                        primary: false,
                    }),
                },
            }),

            debug: new contentConfig.Group({
                options: {
                    info: new contentConfig.Option({
                        passToWebApplication: false,
                        value: false,
                        description:
                            `Print the system debug information. It is recommended to copy the ` +
                            `output of this command when submitting a report regarding any bugs ` +
                            `encountered.`,
                    }),
                    verbose: new contentConfig.Option({
                        passToWebApplication: false,
                        value: false,
                        description: `Increase logs verbosity. Affects both IDE and the backend.`,
                    }),
                    devTools: new contentConfig.Option({
                        passToWebApplication: false,
                        value: false,
                        description: 'Run the application in development mode.',
                    }),
                },
            }),
            chrome: new contentConfig.Group({
                description:
                    `Chrome and Electron command line options. Please be advised that the ` +
                    `provided list contains both Electron-specific options as well as a ` +
                    `selection of Chrome command line options that are officially supported ` +
                    `by Electron ` +
                    `(https://www.electronjs.org/docs/latest/api/command-line-switches). It is ` +
                    `important to note that not all Chrome switches may be compatible with ` +
                    `Electron. For example, the switch '-chrome.crash-test' is not functional in ` +
                    `the Electron environment. For a comprehensive collection of Chrome options, ` +
                    `you may refer to ` +
                    `https://peter.sh/experiments/chromium-command-line-switches.` +
                    `\n\n` +
                    chalk.red(`WARNING: `) +
                    `Neither the option names nor values undergo validation by ` +
                    `Chrome due to the lack of an option validation API. This may result in the ` +
                    `acceptance of invalid options, which will be silently ignored. To verify ` +
                    `the successful passing of options to Chrome, the use of ` +
                    `'-chrome.disable-gpu' can be employed as a diagnostic measure, ` +
                    `effectively preventing the display of WebGL canvas.`,
                options: {
                    // === Electron Options ===
                    // https://www.electronjs.org/docs/latest/api/command-line-switches

                    authServerWhitelist: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: '',
                        description:
                            `A comma-separated list of servers for which integrated ` +
                            `authentication is enabled.`,
                    }),
                    authNegotiateDelegateWhitelist: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: '',
                        description:
                            `A comma-separated list of servers for which delegation of user ` +
                            `credentials is required. Without '*' prefix the URL has to match ` +
                            `exactly.`,
                    }),
                    disableNtlmV2: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: false,
                        description: 'Disables NTLM v2 for posix platforms, no effect elsewhere.',
                    }),
                    disableHttpCache: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: false,
                        description: 'Disables the disk cache for HTTP requests.',
                    }),
                    disableHttp2: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: false,
                        description: 'Disable HTTP/2 and SPDY/3.1 protocols.',
                    }),
                    disableRendererBackgrounding: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: false,
                        description:
                            `Prevents Chrome from lowering the priority of invisible pages' ` +
                            `renderer processes.`,
                    }),
                    diskCacheSize: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: 0,
                        description:
                            `Forces the maximum disk space to be used by the disk cache, ` +
                            `in bytes.`,
                    }),
                    enableLogging: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: '',
                        description:
                            `Prints Chrome's logging to stderr (or a log file, if provided as` +
                            ` argument).`,
                    }),
                    forceFieldtrials: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: '',
                        description:
                            'Field trials to be forcefully enabled or disabled. For example, ' +
                            "'WebRTC-Audio-Red-For-Opus/Enabled/'.",
                    }),
                    hostRules: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: '',
                        description:
                            `A comma-separated list of rules that control how hostnames are ` +
                            `mapped. For example, 'MAP * 127.0.0.1'.`,
                    }),
                    hostResolverRules: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: '',
                        description:
                            `Like '-chrome.host-rules' but these rules only apply to the host ` +
                            `resolver.`,
                    }),
                    ignoreCertificateErrors: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: false,
                        description: `Ignores certificate related errors.`,
                    }),
                    ignoreConnectionsLimit: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: '',
                        description: `Ignore the connections limit for domains list separated by ','.`,
                    }),
                    jsFlags: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: '',
                        description:
                            `Specifies the flags passed to the Node.js engine. For example, ` +
                            `'-chrome.js-flags="--harmony_proxies --harmony_collections"'.`,
                    }),
                    lang: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: '',
                        description: 'Set a custom locale.',
                    }),
                    logFile: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: '',
                        description:
                            `If '-chrome.enable-logging' is specified, logs will be written to ` +
                            `the given path. The parent directory must exist.`,
                    }),
                    logNetLog: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: '',
                        description:
                            'Enable net log events to be saved and writes them to the provided path.',
                    }),
                    logLevel: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: '',
                        description:
                            `Set the verbosity of logging when used together with ` +
                            `'-chrome.enable-logging'. The argument should be one of Chrome's` +
                            ` LogSeverities.`,
                    }),
                    noProxyServer: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: false,
                        description:
                            `Don't use a proxy server and always make direct connections. ` +
                            `Overrides any other proxy server flags that are passed.`,
                    }),
                    noSandbox: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: false,
                        description:
                            `Disable the Chrome sandbox. Forces renderer process and Chrome ` +
                            `helper processes to run un-sandboxed. Should only be used for testing.`,
                    }),
                    proxyBypassList: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: '',
                        description:
                            `Instruct Electron to bypass the proxy server for the given ` +
                            `semi-colon-separated list of hosts. This flag has an effect only if ` +
                            `used in tandem with '-chrome.proxy-server'. For example, ` +
                            `'-chrome.proxy-bypass-list "<local>;*.google.com;*foo.com;1.2.3.4:5678"'.`,
                    }),
                    proxyPacUrl: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: '',
                        description: `Use the PAC script at the specified url.`,
                    }),
                    proxyServer: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: '',
                        description:
                            `Use a specified proxy server ('address:port'), which overrides the` +
                            `system setting. This switch only affects requests with HTTP ` +
                            `protocol, including HTTPS and WebSocket requests. It is also ` +
                            `noteworthy that not all proxy servers support HTTPS and WebSocket ` +
                            `requests. The proxy URL does not support username and password ` +
                            `authentication per ` +
                            `[Chrome issue](https://bugs.chromium.org/p/chromium/issues/detail?id=615947).`,
                    }),
                    remoteDebuggingPort: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: '',
                        description: 'Enables remote debugging over HTTP on the specified port.',
                    }),
                    v: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: 0,
                        description:
                            `Gives the default maximal active V-logging level; 0 is the default. ` +
                            `Normally positive values are used for V-logging levels. This switch ` +
                            `only works when '-chrome.enable-logging' is also passed.`,
                    }),
                    vmodule: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: '',
                        description:
                            `Gives the per-module maximal V-logging levels to override the value ` +
                            `given by '-chrome.v'. E.g. 'my_module=2,foo*=3' would change the ` +
                            `logging level for all code in source files 'my_module.*' and ` +
                            `'foo*.*'. Any pattern containing a forward or backward slash will ` +
                            `be tested against the whole pathname and not only the module. ` +
                            `This switch only works when '-chrome.enable-logging' is also passed.`,
                    }),
                    // Please note that this option uses the snake-case naming convention because
                    // Chrome defines it so.
                    // eslint-disable-next-line @typescript-eslint/naming-convention
                    force_high_performance_gpu: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: false,
                        description:
                            'Force using discrete GPU when there are multiple GPUs available.',
                    }),
                    // Please note that this option uses the snake-case naming convention because
                    // Chrome defines it so.
                    // eslint-disable-next-line @typescript-eslint/naming-convention
                    force_low_power_gpu: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: false,
                        description:
                            'Force using integrated GPU when there are multiple GPUs available.',
                    }),

                    enableBlinkFeatures: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: '',
                        description:
                            `A list of Blink (Chrome's rendering engine) features separated ` +
                            `by ',' like 'CSSVariables,KeyboardEventKey' to enable. The full ` +
                            `list of supported feature strings can be found in the ` +
                            `[RuntimeEnabledFeatures.json5](https://cs.chromium.org/chromium/src/third_party/blink/renderer/platform/runtime_enabled_features.json5?l=70) ` +
                            `file.`,
                    }),

                    disableBlinkFeatures: new contentConfig.Option({
                        passToWebApplication: false,
                        primary: false,
                        value: '',
                        description:
                            `A list of Blink (Chrome's rendering engine) features separated ` +
                            `by ',' like 'CSSVariables,KeyboardEventKey' to disable. The full ` +
                            `list of supported feature strings can be found in the ` +
                            `[RuntimeEnabledFeatures.json5](https://cs.chromium.org/chromium/src/third_party/blink/renderer/platform/runtime_enabled_features.json5?l=70) ` +
                            `file.`,
                    }),
                },
            }),
        },
    })
)
CONFIG.groups.startup.options.platform.value = process.platform

CONFIG.groups.engine.options.preferredVersion.value = BUNDLED_ENGINE_VERSION

export type Args = typeof CONFIG
export type Option<T> = contentConfig.Option<T>
