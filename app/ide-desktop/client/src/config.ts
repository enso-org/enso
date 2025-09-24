/**
 * @file Configuration of the application. It extends the web application configuration with
 * Electron-specific options.
 */

import * as contentConfig from '@/contentConfig'
import * as naming from '@/naming'
import * as paths from '@/paths'

// =================
// === Constants ===
// =================

export const HELP_EXTENDED_NAME = 'helpExtended'
export const HELP_EXTENDED_OPTION_NAME = naming.camelToKebabCase(HELP_EXTENDED_NAME)
const DEFAULT_WIDTH = 1380
const DEFAULT_HEIGHT = 900
const DEFAULT_PORT = 8080
const DEFAULT_PROFILING_TIME = 120

// ==================
// === WindowSize ===
// ==================

/** Window size (width and height). */
export class WindowSize {
  static separator = 'x'
  /** Create a new {@link WindowSize}. */
  constructor(
    public width: number,
    public height: number,
  ) {}

  /** Constructor of the default window size. */
  static default(): WindowSize {
    return new WindowSize(DEFAULT_WIDTH, DEFAULT_HEIGHT)
  }

  /** Parse the input text in form of `<width>x<height>`. */
  static parse(arg: string): Error | WindowSize {
    const size = arg.split(WindowSize.separator)
    const widthStr = size[0]
    const heightStr = size[1]
    const width = widthStr != null ? parseInt(widthStr) : null
    const height = heightStr != null ? parseInt(heightStr) : null
    if (width == null || height == null) {
      return new Error(`Incorrect window size provided '${arg}'.`)
    } else {
      return new WindowSize(width, height)
    }
  }

  /** Return window size in a form of `<width>x<height>`. */
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
          'Show all the configuration options help page, including the less-common ' + 'options.',
      }),
      engine: new contentConfig.Option({
        passToWebApplication: false,
        value: true,
        description: 'Start the engine process.',
      }),
      jvm: new contentConfig.Option({
        passToWebApplication: false,
        value: false,
        description: 'Start engine in JVM mode.',
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
              `Port to use. If the port is unavailable, the next available port is ` + `used.`,
          }),
        },
      }),

      performance: new contentConfig.Group({
        description: `Performance-related configuration options.`,
        options: {
          backgroundThrottling: new contentConfig.Option({
            passToWebApplication: true,
            value: false,
            description: 'Throttle animations when run in background.',
          }),

          forceHighPerformanceGpu: new contentConfig.Option({
            passToWebApplication: false,
            value: true,
            description: 'Force using discrete GPU when there are multiple GPUs available.',
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
              `https://groups.google.com/a/chromium.org/g/chromium-dev/c/` +
              `09NnO6jYT6o.`,
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
            description:
              // This empty string is required for the formatter to keep the line
              // under 100 columns.
              `` + `Enable native CPU-mappable GPU memory buffer support on Linux.`,
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
          profile: new contentConfig.Option({
            passToWebApplication: false,
            value: false,
            description: 'Start backend profiler on startup and log data to a profiling.npss file',
          }),
          profileTime: new contentConfig.Option({
            passToWebApplication: false,
            value: DEFAULT_PROFILING_TIME,
            description:
              'Time since backend startup for which profiling data will be collected, if enabled',
          }),
        },
      }),
    },
  }),
)

CONFIG.groups.engine.options.projectManagerUrl.passToWebApplication = true

/** The type of the full configuration object. */
export type Args = typeof CONFIG
/** A configuration option. */
export type Option<T> = contentConfig.Option<T>
