/** @file Shared application options schema and helpers. */

// =====================
// === Option Schema ===
// =====================

export interface OptionBoolean {
  type: 'boolean'
  defaultValue: boolean
}

export interface OptionNumber {
  type: 'number'
  defaultValue: number
}

export interface OptionString {
  type: 'string'
  defaultValue: string
}

export interface OptionCommon<P extends boolean = boolean> {
  flag: string
  passToWebApplication: P
  description: string
}

export type AnyOption<P extends boolean = boolean> = (OptionBoolean | OptionNumber | OptionString) &
  OptionCommon<P>

export type Option = AnyOption<boolean>

type OptionValue = boolean | number | string
type OptionOf<T extends OptionValue> =
  T extends boolean ? OptionBoolean
  : T extends number ? OptionNumber
  : T extends string ? OptionString
  : never

export function makeOption2<const P extends boolean = true>(args: {
  flag: string
  description: string
  defaultValue: boolean
  passToWebApplication?: P
}): OptionBoolean & OptionCommon<P>
export function makeOption2<const P extends boolean = true>(args: {
  flag: string
  description: string
  defaultValue: number
  passToWebApplication?: P
}): OptionNumber & OptionCommon<P>
export function makeOption2<const P extends boolean = true>(args: {
  flag: string
  description: string
  defaultValue: string
  passToWebApplication?: P
}): OptionString & OptionCommon<P>
export function makeOption2(args: {
  flag: string
  description: string
  defaultValue: boolean | number | string
  passToWebApplication?: boolean
}): Option {
  const { flag, description, defaultValue, passToWebApplication = true } = args
  const common: OptionCommon = { flag, description, passToWebApplication }
  if (typeof defaultValue === 'boolean') {
    return { type: 'boolean', defaultValue, ...common }
  } else if (typeof defaultValue === 'number') {
    return { type: 'number', defaultValue, ...common }
  } else if (typeof defaultValue === 'string') {
    return { type: 'string', defaultValue, ...common }
  } else {
    throw new Error('Invalid default value')
  }
}

// =====================
// === OPTIONS object ===
// =====================

const DEFAULT_PROFILING_TIME = 120
const DEFAULT_PORT = 8080

export const OPTIONS = {
  version: makeOption2({
    flag: '-v, --version',
    defaultValue: false,
    description: 'Print the version',
    passToWebApplication: false,
  }),
  displayWindow: makeOption2({
    flag: '--no-window',
    defaultValue: true,
    description: 'Only the server runs. An alternative client or browser can connect to it',
    passToWebApplication: false,
  }),
  useServer: makeOption2({
    flag: '--no-server',
    defaultValue: true,
    description:
      'When passed, the server will not be run and the application will connect to an existing server on --server.port',
    passToWebApplication: false,
  }),
  engineEnabled: makeOption2({
    flag: '--no-engine',
    defaultValue: true,
    description: 'Do not start the engine process',
    passToWebApplication: false,
  }),
  useJvm: makeOption2({
    flag: '--jvm',
    defaultValue: false,
    description: 'Start engine in JVM mode',
    passToWebApplication: false,
  }),
  startup: {
    project: makeOption2({
      flag: '--startup.project <name>',
      defaultValue: '',
      description:
        'The name of the project to open at startup. If the project does not exist, it will be created',
      passToWebApplication: true,
    }),
    displayedProjectName: makeOption2({
      flag: '--startup.displayedProjectName <name>',
      defaultValue: '',
      description: 'The name of the project to be displayed to the user',
      passToWebApplication: true,
    }),
  },
  authentication: {
    enabled: makeOption2({
      flag: '--authentication.enabled',
      defaultValue: true,
      description:
        'Determines whether user authentication is enabled. This option is always true when executed in the cloud',
      passToWebApplication: true,
    }),
    email: makeOption2({
      flag: '--authentication.email <email>',
      defaultValue: '',
      description: 'The user email, if the user is logged in',
      passToWebApplication: true,
    }),
  },
  server: {
    port: makeOption2({
      flag: '--server.port',
      defaultValue: DEFAULT_PORT,
      description: 'Port to use. If the port is unavailable, the next available port is used',
      passToWebApplication: false,
    }),
  },
  engine: {
    projectManagerPath: makeOption2({
      flag: '--engine.projectManagerPath <path>',
      defaultValue: '', // Client will supply platform-specific default
      description: 'Set the path of a local project manager executable to use for running projects',
      passToWebApplication: false,
    }),
    projectManagerUrl: makeOption2({
      flag: '--engine.projectManagerUrl <url>',
      defaultValue: '',
      description: 'The address of the Project Manager service',
      passToWebApplication: true,
    }),
    ydocUrl: makeOption2({
      flag: '--engine.ydocUrl <url>',
      defaultValue: '',
      description: 'The address of the Ydoc Server endpoint',
      passToWebApplication: true,
    }),
  },
  debug: {
    info: makeOption2({
      flag: '--debug.info',
      defaultValue: false,
      description:
        'Print the system debug information. It is recommended to copy the output of this command when submitting a report regarding any bugs encountered',
      passToWebApplication: false,
    }),
    verbose: makeOption2({
      flag: '--debug.verbose',
      defaultValue: false,
      description: 'Increase logs verbosity. Affects both IDE and the backend',
      passToWebApplication: false,
    }),
    devTools: makeOption2({
      flag: '--debug.devTools',
      defaultValue: false,
      description: 'Run the application in development mode',
      passToWebApplication: false,
    }),
    profile: makeOption2({
      flag: '--debug.profile',
      defaultValue: false,
      description: 'Start backend profiler on startup and log data to a profiling.npss file',
      passToWebApplication: false,
    }),
    profileTime: makeOption2({
      flag: '--debug.profileTime <seconds>',
      defaultValue: DEFAULT_PROFILING_TIME,
      description:
        'Time since backend startup for which profiling data will be collected, if enabled',
      passToWebApplication: false,
    }),
  },
} as const

// =====================
// === Types & Utils ===
// =====================

export type OptionsTree = Option | { [key: string]: OptionsTree }

type ValueOfOption<T extends Option> =
  T extends { type: 'boolean' } ? boolean
  : T extends { type: 'number' } ? number
  : T extends { type: 'string' } ? string
  : never

export type ArgsFromOptions<T extends OptionsTree> =
  T extends Option ? ValueOfOption<T>
  : T extends Record<string, OptionsTree> ? { [K in keyof T]: ArgsFromOptions<T[K]> }
  : never

export function iterateOptions(
  node: OptionsTree,
  onLeaf: (path: string[], opt: Option) => void,
  path: string[] = [],
): void {
  const isLeaf = (n: OptionsTree): n is Option => 'type' in (n as any) && 'flag' in (n as any)
  if (isLeaf(node)) {
    onLeaf(path, node)
    return
  }
  const group = node as Record<string, OptionsTree>
  for (const [key, child] of Object.entries(group) as [string, OptionsTree][]) {
    iterateOptions(child, onLeaf, [...path, key])
  }
}

// Dotted key union for passToWebApplication options (kept simple to avoid deep type instantiation)
export type WebOptionKey = string
export type WebOptionsRecord = Readonly<Record<string, string | number | boolean>>

// ==============================
// === Web Options (URL sync) ===
// ==============================

/** Collect non-default values of passToWebApplication options from parsed args. */
export function collectWebAppOptionsFromArgs<S extends OptionsTree = typeof OPTIONS>(
  args: any,
  schema?: S,
): Partial<WebOptionsRecord> {
  const result: Record<string, string | number | boolean> = {}

  const joinPath = (p: string[]) => p.join('.')
  const get = (obj: any, path: string[]): unknown =>
    path.reduce((o, k) => (o == null ? o : o[k]), obj)

  const effectiveSchema = (schema ?? (OPTIONS as unknown as S)) as unknown as OptionsTree
  iterateOptions(effectiveSchema, (path, node) => {
    if (!node.passToWebApplication) return
    const value = get(args as any, path)
    const key = joinPath(path)
    const serialized = value as any
    const isChanged = serialized !== node.defaultValue
    if (isChanged && serialized !== undefined) {
      result[key] = serialized as any
    }
  })

  return result as Partial<WebOptionsRecord>
}

/** Build URLSearchParams for non-default passToWebApplication options. */
export function buildWebAppURLSearchParamsFromArgs<S extends OptionsTree = typeof OPTIONS>(
  args: any,
  schema?: S,
): URLSearchParams {
  const params = new URLSearchParams()
  const entries = collectWebAppOptionsFromArgs(args, schema)
  for (const [key, val] of Object.entries(entries)) params.append(key, String(val))
  return params
}

/** Parse web options from URLSearchParams into a typed record. */
export function parseWebAppOptionsFromSearchParams<S extends OptionsTree = typeof OPTIONS>(
  params: URLSearchParams,
  schema?: S,
): Partial<WebOptionsRecord> {
  const out: Record<string, string | number | boolean> = {}
  const coerceBoolean = (v: string): boolean | undefined => {
    switch (v.toLowerCase()) {
      case 'true':
      case '1':
      case 'yes':
      case 'enabled':
        return true
      case 'false':
      case '0':
      case 'no':
      case 'disabled':
        return false
      default:
        return undefined
    }
  }
  const joinPath = (p: string[]) => p.join('.')
  const effectiveSchema = (schema ?? (OPTIONS as unknown as S)) as unknown as OptionsTree
  iterateOptions(effectiveSchema, (path, node) => {
    if (!node.passToWebApplication) return
    const key = joinPath(path)
    const value = params.get(key)
    if (value == null) return
    if (node.type === 'boolean') {
      const b = coerceBoolean(value)
      if (b != null) out[key] = b
    } else if (node.type === 'number') {
      const n = Number(value)
      if (!Number.isNaN(n)) out[key] = n
    } else {
      out[key] = value
    }
  })
  return out as Partial<WebOptionsRecord>
}
