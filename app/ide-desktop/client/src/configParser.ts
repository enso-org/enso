/** @file Command line options parser. */

import * as config from '@/config'
import * as fileAssociations from '@/fileAssociations'
import * as naming from '@/naming'
import * as paths from '@/paths'
import { Command, InvalidArgumentError, program } from 'commander'

const DEFAULT_PROFILING_TIME = 120
const DEFAULT_PORT = 8080

// =====================
// === Option Parser ===
// =====================

/** Parse command line arguments. */
export function parseArgs(clientArgs: readonly string[] = fileAssociations.CLIENT_ARGUMENTS) {
  const args = config.CONFIG
  const argv = clientArgs
  const yargsOptions = args.optionsRecursive().reduce((opts: Record<string, Options>, option) => {
    opts[naming.camelToKebabCase(option.qualifiedName())] = {
      ...option,
      requiresArg: ['string', 'array'].includes(option.type),
      default: null,
      // Required because yargs defines `defaultDescription`
      // as `string | undefined`, not `string | null`.
      defaultDescription: option.defaultDescription ?? undefined,
    }
    return opts
  }, {})

  interface OptionBoolean {
    type: 'boolean'
    defaultValue: boolean
  }

  interface OptionNumber {
    type: 'number'
    defaultValue: number
  }

  interface OptionString {
    type: 'string'
    defaultValue: string
  }

  interface OptionCommon {
    flag: string
    passToWebApplication: boolean
    description: string
  }

  type Option = (OptionBoolean | OptionNumber | OptionString) & OptionCommon

  function makeOption({
    flag,
    passToWebApplication = true,
    description,
    defaultValue,
  }: {
    flag: string
    passToWebApplication?: boolean
    description: string
    defaultValue: boolean | number | string
  }): Option {
    const common: OptionCommon = {
      flag,
      description,
      passToWebApplication,
    }
    if (typeof defaultValue === 'boolean') {
      return {
        type: 'boolean',
        defaultValue,
        ...common,
      }
    } else if (typeof defaultValue === 'number') {
      return {
        type: 'number',
        defaultValue,
        ...common,
      }
    } else if (typeof defaultValue === 'string') {
      return {
        type: 'string',
        defaultValue,
        ...common,
      }
    } else {
      throw new Error('Invalid default value')
    }
  }

  const options = {
    displayWindow: makeOption({
      flag: '--window',
      defaultValue: true,
      description:
        'Display the window. When set to false, only the server runs. An alternative client or browser can connect to it.',
      passToWebApplication: false,
    }),
    useServer: makeOption({
      flag: '--server',
      defaultValue: true,
      description:
        'Run the server. When set to false, connect to an existing server on the provided port.',
      passToWebApplication: false,
    }),
    useEngine: makeOption({
      flag: '--engine',
      defaultValue: true,
      description: 'Start the engine process.',
      passToWebApplication: false,
    }),
    useJvm: makeOption({
      flag: '--jvm',
      defaultValue: false,
      description: 'Start engine in JVM mode.',
      passToWebApplication: false,
    }),
    startup: {
      project: makeOption({
        flag: '--startup.project',
        defaultValue: '',
        description:
          'The name of the project to open at startup. If the project does not exist, it will be created.',
        passToWebApplication: true,
      }),
      displayedProjectName: makeOption({
        flag: '--startup.displayedProjectName',
        defaultValue: '',
        description: 'The name of the project to be displayed to the user.',
        passToWebApplication: true,
      }),
    },
    authentication: {
      enabled: makeOption({
        flag: '--authentication.enabled',
        defaultValue: true,
        description:
          'Determines whether user authentication is enabled. This option is always true when executed in the cloud.',
        passToWebApplication: true,
      }),
      email: makeOption({
        flag: '--authentication.email',
        defaultValue: '',
        description: 'The user email, if the user is logged in.',
        passToWebApplication: true,
      }),
    },
    window: {
      size: makeOption({
        flag: '--window.size',
        defaultValue: config.WindowSize.default().pretty(),
        description: 'Set the initial window size.',
        passToWebApplication: false,
      }),
      closeToQuit: makeOption({
        flag: '--window.closeToQuit',
        defaultValue: process.platform !== 'darwin',
        description: 'Determine whether the app should quit when the window is closed.',
        passToWebApplication: false,
      }),
    },
    server: {
      port: makeOption({
        flag: '--server.port',
        defaultValue: DEFAULT_PORT,
        description: 'Port to use. If the port is unavailable, the next available port is used.',
        passToWebApplication: false,
      }),
    },
    engine: {
      projectManagerPath: makeOption({
        flag: '--engine.projectManagerPath',
        defaultValue: paths.PROJECT_MANAGER_PATH,
        description:
          'Set the path of a local project manager executable to use for running projects.',
        passToWebApplication: false,
      }),
      projectManagerUrl: makeOption({
        flag: '--engine.projectManagerUrl',
        defaultValue: '',
        description: 'The address of the Project Manager service.',
        passToWebApplication: true,
      }),
      ydocUrl: makeOption({
        flag: '--engine.ydocUrl',
        defaultValue: '',
        description: 'The address of the Ydoc Server endpoint.',
        passToWebApplication: true,
      }),
    },
    debug: {
      info: makeOption({
        flag: '--debug.info',
        defaultValue: false,
        description:
          'Print the system debug information. It is recommended to copy the output of this command when submitting a report regarding any bugs encountered.',
        passToWebApplication: false,
      }),
      verbose: makeOption({
        flag: '--debug.verbose',
        defaultValue: false,
        description: 'Increase logs verbosity. Affects both IDE and the backend.',
        passToWebApplication: false,
      }),
      devTools: makeOption({
        flag: '--debug.devTools',
        defaultValue: false,
        description: 'Run the application in development mode.',
        passToWebApplication: false,
      }),
      profile: makeOption({
        flag: '--debug.profile',
        defaultValue: false,
        description: 'Start backend profiler on startup and log data to a profiling.npss file',
        passToWebApplication: false,
      }),
      profileTime: makeOption({
        flag: '--debug.profileTime',
        defaultValue: DEFAULT_PROFILING_TIME,
        description:
          'Time since backend startup for which profiling data will be collected, if enabled',
        passToWebApplication: false,
      }),
    },
  }

  function parseNumber(value: string, _: number): number {
    const parsed = parseInt(value)
    if (isNaN(parsed)) {
      throw new InvalidArgumentError('expected a number')
    }
    return parsed
  }

  function addOptions(program: Command, option: Option | Record<string, Option>) {
    if ('type' in option) {
      switch (option.type) {
        case 'boolean':
          program.option(option.flag, option.description, option.defaultValue)
          break
        case 'number':
          program.option(option.flag, option.description, parseNumber, option.defaultValue)
          break
        case 'string':
          program.option(option.flag, option.description, option.defaultValue)
          break
      }
    } else {
      for (const [optionName, opt] of Object.entries(option as Record<string, Option>)) {
        addOptions(program, opt)
      }
    }
  }

  addOptions(program, options)

  program.parse(argv)

  const opts = program.opts()

  // let windowSize = config.WindowSize.default()
  // const providedWindowSize = populatedOptions['window.size'].value
  // const parsedWindowSize = config.WindowSize.parse(providedWindowSize)

  // if (parsedWindowSize instanceof Error) {
  //   console.error(`Wrong window size provided: '${providedWindowSize}'.`)
  // } else {
  //   windowSize = parsedWindowSize
  // }

  return { args, windowSize }
}
