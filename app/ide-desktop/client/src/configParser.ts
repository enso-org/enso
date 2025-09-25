/** @file Command line options parser. */

import * as config from '@/config'
import * as fileAssociations from '@/fileAssociations'
import * as paths from '@/paths'
import { Command, InvalidArgumentError } from 'commander'

const DEFAULT_PROFILING_TIME = 120
const DEFAULT_PORT = 8080

// =====================
// === Option Parser ===
// =====================

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

interface OptionCommon<P extends boolean = boolean> {
  flag: string
  passToWebApplication: P
  description: string
}

type AnyOption<P extends boolean = boolean> = (OptionBoolean | OptionNumber | OptionString) &
  OptionCommon<P>

type Option = AnyOption<boolean>

// Option factory with overloads that preserve passToWebApplication literal type
function makeOption(
  args: {
    flag: string
    passToWebApplication?: boolean
    description: string
    defaultValue: boolean
  } & { passToWebApplication?: undefined },
): OptionBoolean & OptionCommon<true>
function makeOption(args: {
  flag: string
  passToWebApplication: true
  description: string
  defaultValue: boolean
}): OptionBoolean & OptionCommon<true>
function makeOption(args: {
  flag: string
  passToWebApplication: false
  description: string
  defaultValue: boolean
}): OptionBoolean & OptionCommon<false>
function makeOption(
  args: {
    flag: string
    passToWebApplication?: boolean
    description: string
    defaultValue: number
  } & { passToWebApplication?: undefined },
): OptionNumber & OptionCommon<true>
function makeOption(args: {
  flag: string
  passToWebApplication: true
  description: string
  defaultValue: number
}): OptionNumber & OptionCommon<true>
function makeOption(args: {
  flag: string
  passToWebApplication: false
  description: string
  defaultValue: number
}): OptionNumber & OptionCommon<false>
function makeOption(
  args: {
    flag: string
    passToWebApplication?: boolean
    description: string
    defaultValue: string
  } & { passToWebApplication?: undefined },
): OptionString & OptionCommon<true>
function makeOption(args: {
  flag: string
  passToWebApplication: true
  description: string
  defaultValue: string
}): OptionString & OptionCommon<true>
function makeOption(args: {
  flag: string
  passToWebApplication: false
  description: string
  defaultValue: string
}): OptionString & OptionCommon<false>
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

export const OPTIONS = {
  version: makeOption({
    flag: '-v, --version',
    defaultValue: false,
    description: 'Print the version',
    passToWebApplication: false,
  }),
  displayWindow: makeOption({
    flag: '--no-window',
    defaultValue: true,
    description: 'Only the server runs. An alternative client or browser can connect to it',
    passToWebApplication: false,
  }),
  useServer: makeOption({
    flag: '--no-server',
    defaultValue: true,
    description:
      'When passed, the server will not be run and the application will connect to an existing server on --server.port',
    passToWebApplication: false,
  }),
  engineEnabled: makeOption({
    flag: '--no-engine',
    defaultValue: true,
    description: 'Do not start the engine process',
    passToWebApplication: false,
  }),
  useJvm: makeOption({
    flag: '--jvm',
    defaultValue: false,
    description: 'Start engine in JVM mode',
    passToWebApplication: false,
  }),
  startup: {
    project: makeOption({
      flag: '--startup.project <name>',
      defaultValue: '',
      description:
        'The name of the project to open at startup. If the project does not exist, it will be created',
      passToWebApplication: true,
    }),
    displayedProjectName: makeOption({
      flag: '--startup.displayedProjectName <name>',
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
        'Determines whether user authentication is enabled. This option is always true when executed in the cloud',
      passToWebApplication: true,
    }),
    email: makeOption({
      flag: '--authentication.email <email>',
      defaultValue: '',
      description: 'The user email, if the user is logged in',
      passToWebApplication: true,
    }),
  },
  window: {
    size: makeOption({
      flag: '--window.size <size>',
      defaultValue: config.WindowSize.default().pretty(),
      description: 'Set the initial window size',
      passToWebApplication: false,
    }),
    closeToQuit: makeOption({
      flag: '--window.closeToQuit',
      defaultValue: process.platform !== 'darwin',
      description:
        'Determine whether the app should quit when the window is closed. false on MacOS, true on other platforms',
      passToWebApplication: false,
    }),
  },
  server: {
    port: makeOption({
      flag: '--server.port',
      defaultValue: DEFAULT_PORT,
      description: 'Port to use. If the port is unavailable, the next available port is used',
      passToWebApplication: false,
    }),
  },
  engine: {
    projectManagerPath: makeOption({
      flag: '--engine.projectManagerPath <path>',
      defaultValue: paths.PROJECT_MANAGER_PATH,
      description: 'Set the path of a local project manager executable to use for running projects',
      passToWebApplication: false,
    }),
    projectManagerUrl: makeOption({
      flag: '--engine.projectManagerUrl <url>',
      defaultValue: '',
      description: 'The address of the Project Manager service',
      passToWebApplication: true,
    }),
    ydocUrl: makeOption({
      flag: '--engine.ydocUrl <url>',
      defaultValue: '',
      description: 'The address of the Ydoc Server endpoint',
      passToWebApplication: true,
    }),
  },
  debug: {
    info: makeOption({
      flag: '--debug.info',
      defaultValue: false,
      description:
        'Print the system debug information. It is recommended to copy the output of this command when submitting a report regarding any bugs encountered',
      passToWebApplication: false,
    }),
    verbose: makeOption({
      flag: '--debug.verbose',
      defaultValue: false,
      description: 'Increase logs verbosity. Affects both IDE and the backend',
      passToWebApplication: false,
    }),
    devTools: makeOption({
      flag: '--debug.devTools',
      defaultValue: false,
      description: 'Run the application in development mode',
      passToWebApplication: false,
    }),
    profile: makeOption({
      flag: '--debug.profile',
      defaultValue: false,
      description: 'Start backend profiler on startup and log data to a profiling.npss file',
      passToWebApplication: false,
    }),
    profileTime: makeOption({
      flag: '--debug.profileTime <seconds>',
      defaultValue: DEFAULT_PROFILING_TIME,
      description:
        'Time since backend startup for which profiling data will be collected, if enabled',
      passToWebApplication: false,
    }),
  },
}

// =====================
// === Types & Utils ===
// =====================

type OptionsTree = Option | { [key: string]: OptionsTree }

type ValueOfOption<T extends Option> =
  T extends { type: 'boolean' } ? boolean
  : T extends { type: 'number' } ? number
  : T extends { type: 'string' } ? string
  : never

export type ArgsFromOptions<T extends OptionsTree> =
  T extends Option ? ValueOfOption<T>
  : T extends Record<string, OptionsTree> ? { [K in keyof T]: ArgsFromOptions<T[K]> }
  : never

type WithParsedWindowSize<T> =
  T extends { window: infer W } ?
    Omit<T, 'window'> &
      (W extends { size: any } ? { window: Omit<W, 'size'> & { size: config.WindowSize } }
      : { window: W })
  : T

export type ParsedArgs = WithParsedWindowSize<ArgsFromOptions<typeof OPTIONS>>

function joinPath(path: string[]) {
  return path.join('.')
}

function isLeafOption(node: OptionsTree): node is Option {
  return (
    (typeof (node as any)?.flag === 'string' &&
      typeof (node as any)?.description === 'string' &&
      (node as any)?.type === 'boolean') ||
    (node as any)?.type === 'number' ||
    (node as any)?.type === 'string'
  )
}

function iterateOptions(
  node: OptionsTree,
  onLeaf: (path: string[], opt: Option) => void,
  path: string[] = [],
): void {
  if (isLeafOption(node)) {
    onLeaf(path, node)
    return
  }
  const group = node as Record<string, OptionsTree>
  for (const [key, child] of Object.entries(group) as [string, OptionsTree][]) {
    iterateOptions(child, onLeaf, [...path, key])
  }
}

/** Parse command line arguments. */
export function parseArgs(
  clientArgs: readonly string[] = fileAssociations.CLIENT_ARGUMENTS,
): ParsedArgs {
  const argv = clientArgs

  function parseNumber(value: string, _: number): number {
    const parsed = parseInt(value)
    if (isNaN(parsed)) {
      throw new InvalidArgumentError('expected a number')
    }
    return parsed
  }

  const command = new Command()

  // Map from logical option path (e.g. "window.size") to Commander attribute name used in opts().
  const optionPathToAttrName = new Map<string, string>()

  // Register Commander options using the iterator
  iterateOptions(OPTIONS, (path, node) => {
    const opt = command.createOption(node.flag, node.description)
    if (node.type === 'number') opt.argParser(parseNumber)
    opt.default(node.defaultValue)
    command.addOption(opt)
    optionPathToAttrName.set(joinPath(path), opt.attributeName())
  })

  command.parse(argv, { from: 'user' })
  const raw = command.opts<Record<string, unknown>>()

  function buildArgs<T extends OptionsTree>(node: T, path: string[] = []): ArgsFromOptions<T> {
    if (isLeafOption(node)) {
      const attrName = optionPathToAttrName.get(joinPath(path))
      const value = attrName ? raw[attrName] : undefined
      switch (node.type) {
        case 'boolean':
          return (typeof value === 'boolean' ? value : (
            node.defaultValue
          )) as unknown as ArgsFromOptions<T>
        case 'number':
          return (typeof value === 'number' ? value : (
            node.defaultValue
          )) as unknown as ArgsFromOptions<T>
        case 'string':
          return (typeof value === 'string' ? value : (
            node.defaultValue
          )) as unknown as ArgsFromOptions<T>
      }
    }
    const out: Record<string, unknown> = {}
    const group = node as unknown as Record<string, OptionsTree>
    for (const [key, child] of Object.entries(group) as [string, OptionsTree][]) {
      out[key] = buildArgs(child, [...path, key])
    }
    return out as ArgsFromOptions<T>
  }

  const argsBase = buildArgs(OPTIONS)

  // Convert window.size from string to config.WindowSize at parse-time.
  const providedWindowSize = (argsBase as any).window.size as string
  const parsedWindowSize = config.WindowSize.parse(providedWindowSize)
  const finalWindowSize =
    parsedWindowSize instanceof Error ?
      (console.error(`Wrong window size provided: '${providedWindowSize}'.`),
      config.WindowSize.default())
    : parsedWindowSize

  const args: ParsedArgs = {
    ...(argsBase as any),
    window: {
      ...(argsBase as any).window,
      size: finalWindowSize,
    },
  }

  return args
}

// ==============================
// === Web Options (URL sync) ===
// ==============================

// Compute union of dotted paths to leaf options that are passToWebApplication: true
type WebOptionPaths<T, Prefix extends string = ''> =
  T extends AnyOption<infer P> ?
    P extends true ?
      Prefix
    : never
  : {
      [K in keyof T & string]: WebOptionPaths<T[K], Prefix extends '' ? `${K}` : `${Prefix}.${K}`>
    }[keyof T & string]

// Value at dotted path P in object T
type PathValue<T, P extends string> =
  P extends `${infer K}.${infer Rest}` ?
    K extends keyof T ?
      PathValue<T[K], Rest>
    : never
  : P extends keyof T ? T[P]
  : never

// All passToWebApplication option keys and their values (based on original ArgsFromOptions types)
export type WebOptionKey = WebOptionPaths<typeof OPTIONS>
export type WebOptionsRecord = {
  [K in WebOptionKey]: PathValue<ArgsFromOptions<typeof OPTIONS>, K>
}

/** Collect non-default values of passToWebApplication options from parsed args. */
export function collectWebAppOptions(args: ParsedArgs): Partial<WebOptionsRecord> {
  const result: Record<string, string | number | boolean> = {}

  function getValueAtPath(obj: any, path: string[]): unknown {
    let cur = obj
    for (const segment of path) {
      if (cur == null) return undefined
      cur = cur[segment]
    }
    return cur
  }

  iterateOptions(OPTIONS, (path, node) => {
    if (!node.passToWebApplication) return
    const value = getValueAtPath(args, path)
    // Normalize value to serialized form for comparison and output.
    const key = joinPath(path)
    let serialized: string | number | boolean | undefined
    if (key === 'window.size' && value && typeof (value as any).pretty === 'function') {
      serialized = (value as any).pretty() as string
    } else {
      serialized = value as any
    }
    // Compare to default to include only when changed by user
    const isChanged = serialized !== node.defaultValue
    if (isChanged && serialized !== undefined) {
      result[key] = serialized as any
    }
  })

  return result as Partial<WebOptionsRecord>
}

/** Build URLSearchParams for non-default passToWebApplication options. */
export function buildWebAppURLSearchParams(args: ParsedArgs): URLSearchParams {
  const params = new URLSearchParams()
  const entries = collectWebAppOptions(args)
  for (const [key, val] of Object.entries(entries)) {
    params.append(key, String(val))
  }
  return params
}
