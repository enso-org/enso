/** @file Shared application options schema and helpers. */
import { z } from 'zod'

const DEFAULT_PROFILING_TIME = 120
const DEFAULT_PORT = 8080

/** Schema for app-wide configuration options. */
export const OptionsSchema = z.object({
  version: z.boolean().default(false),
  headless: z.boolean().default(false),
  displayWindow: z.boolean().default(true),
  useServer: z.boolean().default(true),
  engineEnabled: z.boolean().default(true),
  useJvm: z.boolean().default(false),
  startup: z
    .object({
      project: z.string().default(''),
    })
    .default({}),
  authentication: z
    .object({
      enabled: z.boolean().default(true),
      email: z.string().default(''),
    })
    .default({}),
  server: z
    .object({
      port: z.number().int().default(DEFAULT_PORT),
    })
    .default({}),
  engine: z
    .object({
      ydocUrl: z.string().default(''),
    })
    .default({}),
  debug: z
    .object({
      info: z.boolean().default(false),
      verbose: z.boolean().default(false),
      devTools: z.boolean().default(false),
      profile: z.boolean().default(false),
      profileTime: z.number().int().default(DEFAULT_PROFILING_TIME),
    })
    .default({}),
})

/** Global configuration of Enso Studio, parsed from CLI arguments and passed to web application via URL params. */
export type Options = z.infer<typeof OptionsSchema>

/** Default values for all configuration options. */
export function defaultOptions(): Options {
  return OptionsSchema.parse({})
}

/**
 * Dotted keys which should be synced to the web application via URL params.
 * Other options are only used by Electron and not passed to the web application.
 */
export const PASS_TO_WEB: ReadonlySet<string> = new Set([
  'startup.project',
  'authentication.enabled',
  'authentication.email',
  'engine.ydocUrl',
])

/** Possible values for options. */
export type OptionValue = string | number | boolean

const phantomKey = Symbol('flattened')
type Flat<T> = Record<string, OptionValue> & { readonly [phantomKey]?: T }

/** Flatten a nested object into a dotted-key record. */
export function flattenObject<T extends object>(obj: T, prefix = ''): Flat<T> {
  const out: Flat<T> = {}
  if (obj != null) {
    for (const [k, v] of Object.entries(obj)) {
      const key = prefix ? `${prefix}.${k}` : k
      if (v != null && typeof v === 'object' && !Array.isArray(v)) {
        Object.assign(out, flattenObject(v, key))
      } else {
        out[key] = v
      }
    }
  }
  return out
}

/** Turn a flat dotted-record into a nested object. */
export function unflattenObject<T extends object>(flat: Flat<T>): T {
  const out: any = {}
  for (const [k, v] of Object.entries(flat)) {
    const pathSegments = k.split('.')
    let cur = out
    for (const segment of pathSegments.slice(0, -1)) {
      cur[segment] ??= {}
      cur = cur[segment]
    }
    const last = pathSegments[pathSegments.length - 1]!
    cur[last] = v
  }
  return out as T
}

/** Build URLSearchParams for non-default pass-to-web options. */
export function buildWebAppURLSearchParamsFromArgs(args: Options): URLSearchParams {
  const params = new URLSearchParams()
  const entries = collectWebAppOptionsFromArgs(args)
  Object.entries(entries).forEach(([key, val]) => params.append(key, String(val)))
  return params
}

/** Collect non-default values of pass-to-web options from parsed args. */
function collectWebAppOptionsFromArgs(options: Options): Record<string, string | number | boolean> {
  const result: Record<string, string | number | boolean> = {}
  const defaults = defaultOptions()
  const flatDefaults = flattenObject(defaults)
  const flatOptions = flattenObject(options)
  for (const key of PASS_TO_WEB) {
    const value = flatOptions[key]
    if (value == null)
      throw new Error(`Option ${key} not found, but needs to be passed to web application`)
    const def = flatDefaults[key]
    if (value != null && JSON.stringify(value) !== JSON.stringify(def)) {
      result[key] = value
    }
  }
  return result
}

/** Parse pass-to-web options from URLSearchParams. For missing values, defaults are used. */
export function parseWebAppOptionsFromSearchParams(params: URLSearchParams): Options {
  const out = flattenObject(defaultOptions())
  for (const key of PASS_TO_WEB) {
    const value = params.get(key)
    if (value == null) continue
    const def = out[key]
    switch (typeof def) {
      case 'boolean': {
        const b = coerceBoolean(value)
        if (b != null) {
          out[key] = b
        } else {
          console.warn(`Invalid boolean value for option ${key}: ${value}, using default ${def}.`)
        }
        break
      }
      case 'number': {
        const n = Number(value)
        if (!Number.isNaN(n)) {
          out[key] = n
        } else {
          console.warn(`Invalid number value for option ${key}: ${value}, using default ${def}.`)
        }
        break
      }
      case 'string': {
        out[key] = value
        break
      }
      default:
        throw new Error(`Invalid option type for option ${key}: ${typeof def}.`)
    }
  }
  return unflattenObject<Options>(out)
}

/**
 * Coerce a string to a boolean.
 *
 * true, 1, yes, enabled → true
 * false, 0, no, disabled → false
 * anything else → undefined
 */
function coerceBoolean(v: string): boolean | undefined {
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
