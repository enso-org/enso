/** @file Command line options parser. */

import { Command, InvalidArgumentError } from 'commander'
import {
  OptionsSchema,
  buildWebAppURLSearchParamsFromArgs as _buildWebAppURLSearchParamsFromArgs,
  collectWebAppOptionsFromArgs as _collectWebAppOptionsFromArgs,
  defaultOptions,
  flattenObject,
  unflattenObject,
  type Options,
} from 'enso-common/src/options'

// CLI-only metadata defining flags and descriptions.
const OPTIONS_META: Readonly<Record<string, { flag: string; description: string }>> = {
  version: { flag: '-v, --version', description: 'Print the version' },
  displayWindow: {
    flag: '--no-window',
    description: 'Only the server runs. An alternative client or browser can connect to it',
  },
  useServer: {
    flag: '--no-server',
    description:
      'When passed, the server will not be run and the application will connect to an existing server on --server.port',
  },
  engineEnabled: { flag: '--no-engine', description: 'Do not start the engine process' },
  useJvm: { flag: '--jvm', description: 'Start engine in JVM mode' },
  'startup.project': {
    flag: '--startup.project <name>',
    description:
      'The name of the project to open at startup. If the project does not exist, it will be created',
  },
  'startup.displayedProjectName': {
    flag: '--startup.displayedProjectName <name>',
    description: 'The name of the project to be displayed to the user',
  },
  'authentication.enabled': {
    flag: '--authentication.enabled',
    description:
      'Determines whether user authentication is enabled. This option is always true when executed in the cloud',
  },
  'authentication.email': {
    flag: '--authentication.email <email>',
    description: 'The user email, if the user is logged in',
  },
  'server.port': {
    flag: '--server.port',
    description: 'Port to use. If the port is unavailable, the next available port is used',
  },
  'engine.projectManagerPath': {
    flag: '--engine.projectManagerPath <path>',
    description: 'Set the path of a local project manager executable to use for running projects',
  },
  'engine.projectManagerUrl': {
    flag: '--engine.projectManagerUrl <url>',
    description: 'The address of the Project Manager service',
  },
  'engine.ydocUrl': {
    flag: '--engine.ydocUrl <url>',
    description: 'The address of the Ydoc Server endpoint',
  },
  'debug.info': {
    flag: '--debug.info',
    description:
      'Print the system debug information. It is recommended to copy the output of this command when submitting a report regarding any bugs encountered',
  },
  'debug.verbose': {
    flag: '--debug.verbose',
    description: 'Increase logs verbosity. Affects both IDE and the backend',
  },
  'debug.devTools': {
    flag: '--debug.devTools',
    description: 'Run the application in development mode',
  },
  'debug.profile': {
    flag: '--debug.profile',
    description: 'Start backend profiler on startup and log data to a profiling.npss file',
  },
  'debug.profileTime': {
    flag: '--debug.profileTime <seconds>',
    description:
      'Time since backend startup for which profiling data will be collected, if enabled',
  },
} as const

// =====================
// === Types & Utils ===
// =====================

export type ParsedArgs = Options

// (no-op helper removed)

/** Parse command line arguments. */
/** Parse command line arguments to validated options. */
export function parseArgs(argv: readonly string[]): ParsedArgs {
  const command = new Command()

  function parseNumber(value: string, _: number): number {
    const parsed = parseInt(value)
    if (isNaN(parsed)) {
      throw new InvalidArgumentError('expected a number')
    }
    return parsed
  }

  // Register options based on defaults and metadata.
  const defaults = defaultOptions()
  const flatDefaults = flattenObject(defaults)
  const optionPathToAttrName = new Map<string, string>()
  for (const [path, meta] of Object.entries(OPTIONS_META)) {
    const def = flatDefaults[path]
    const opt = command.createOption(meta.flag, meta.description)
    if (typeof def === 'number') opt.argParser(parseNumber)
    opt.default(def)
    command.addOption(opt)
    optionPathToAttrName.set(path, opt.attributeName())
  }

  command.parse(argv, { from: 'user' })
  const raw = command.opts<Record<string, unknown>>()

  // Materialize values by merging raw opts into defaults via the attribute-name map.
  const mergedFlat: Record<string, unknown> = { ...flatDefaults }
  for (const [path, attr] of optionPathToAttrName.entries()) {
    if (attr in raw) mergedFlat[path] = raw[attr]
  }
  const candidate = unflattenObject<Options>(mergedFlat)
  return OptionsSchema.parse(candidate)
}

// ==============================
// === Web Options (URL sync) ===
// ==============================

/** Collect non-default pass-to-web options from parsed args. */
export function collectWebAppOptions(args: ParsedArgs): Record<string, string | number | boolean> {
  return _collectWebAppOptionsFromArgs(args) as any
}

/** Build URLSearchParams for non-default pass-to-web options. */
export function buildWebAppURLSearchParams(args: ParsedArgs): URLSearchParams {
  return _buildWebAppURLSearchParamsFromArgs(args)
}
