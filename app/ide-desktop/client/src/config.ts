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

export const CONFIG = new contentConfig.Group({
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
    startup: new contentConfig.Group({
      description: undefined,
      groups: {},
      options: {
        project: new contentConfig.Option({
          description:
            'The name of the project to open at startup. If the project does not exist, it will be created.',
          value: '',
        }),
        displayedProjectName: new contentConfig.Option({
          description: 'The name of the project to be displayed to the user.',
          value: '',
          primary: false,
        }),
      },
    }),
    authentication: new contentConfig.Group({
      description: 'Options to manage application authentication properties.',
      groups: {},
      options: {
        enabled: new contentConfig.Option({
          description:
            'Determines whether user authentication is enabled. This option is always true when executed in the cloud.',
          value: true,
        }),
        email: new contentConfig.Option({
          description: 'The user email, if the user is logged in.',
          value: '',
          primary: false,
        }),
      },
    }),
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

    engine: new contentConfig.Group({
      description: 'Options that control the Enso Engine, the data processing backend.',
      options: {
        projectManagerPath: new contentConfig.Option({
          passToWebApplication: false,
          value: paths.PROJECT_MANAGER_PATH,
          description:
            'Set the path of a local project manager executable to use for ' + 'running projects.',
          primary: false,
        }),
        projectManagerUrl: new contentConfig.Option({
          description: 'The address of the Project Manager service.',
          value: '',
          primary: false,
        }),
        ydocUrl: new contentConfig.Option({
          description: 'The address of the Ydoc Server endpoint.',
          value: '',
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
})

/** The type of the full configuration object. */
export type Args = typeof CONFIG
/** A configuration option. */
export type Option<T> = contentConfig.Option<T>
