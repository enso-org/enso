/** @file Shared application options schema and helpers. */

import { z } from 'zod'

// =====================
// === Zod Schema ======
// =====================

const DEFAULT_PROFILING_TIME = 120
const DEFAULT_PORT = 8080

export const OptionsSchema = z.object({
  version: z.boolean().default(false),
  displayWindow: z.boolean().default(true),
  useServer: z.boolean().default(true),
  engineEnabled: z.boolean().default(true),
  useJvm: z.boolean().default(false),
  startup: z
    .object({
      project: z.string().default(''),
      displayedProjectName: z.string().default(''),
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
      projectManagerPath: z.string().default(''),
      projectManagerUrl: z.string().default(''),
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

export type Options = z.infer<typeof OptionsSchema>

/** Return fully defaulted options. */
export function defaultOptions(): Options {
  return OptionsSchema.parse({})
}

// =====================
// === Pass-to-web ====
// =====================

/** Dotted keys which should be synced to the web application via URL params. */
export const PASS_TO_WEB: ReadonlySet<string> = new Set([
  'startup.project',
  'startup.displayedProjectName',
  'authentication.enabled',
  'authentication.email',
  'engine.projectManagerUrl',
  'engine.ydocUrl',
])

// =====================
// === Flat helpers  ===
// =====================

type Flat = Record<string, unknown>

/** Flatten a nested object into a dotted-key record. */
export function flattenObject(obj: unknown, prefix = ''): Flat {
  const out: Flat = {}
  if (obj && typeof obj === 'object') {
    for (const [k, v] of Object.entries(obj as Record<string, unknown>)) {
      const key = prefix ? `${prefix}.${k}` : k
      if (v && typeof v === 'object' && !Array.isArray(v)) {
        Object.assign(out, flattenObject(v, key))
      } else {
        out[key] = v
      }
    }
  }
  return out
}

/** Turn a flat dotted-record into a nested object. */
export function unflattenObject<T extends object>(flat: Flat): T {
  const out: any = {}
  for (const [k, v] of Object.entries(flat)) {
    const parts = k.split('.')
    let cur = out
    for (let i = 0; i < parts.length - 1; i++) {
      const segment = parts[i]!
      cur[segment] ??= {}
      cur = cur[segment]
    }
    const last = parts[parts.length - 1]!
    cur[last] = v
  }
  return out as T
}

/** Merge defaults with flat dotted overrides and validate. */
export function mergeDefaultsWithFlat(overrides: Record<string, unknown>): Options {
  const flat = { ...flattenObject(defaultOptions()), ...overrides }
  return OptionsSchema.parse(unflattenObject<Options>(flat))
}

/** Return flat keys that differ between defaults and value. */
export function diffFromDefaults(defaults: object, value: object): Flat {
  const a = flattenObject(defaults)
  const b = flattenObject(value)
  const out: Flat = {}
  for (const key of Object.keys(b)) {
    if (JSON.stringify(b[key]) !== JSON.stringify(a[key])) out[key] = b[key]
  }
  return out
}

// Keep the export name `OPTIONS` for existing imports. It holds the schema.
export const OPTIONS = OptionsSchema as unknown as any

// Legacy type aliases retained for compatibility
export type OptionsTree = unknown
export type ArgsFromOptions<_T> = Options
export type WebOptionKey = string
export type WebOptionsRecord = Record<string, string | number | boolean>

// ==============================
// === Web Options (URL sync) ===
// ==============================

/** Collect non-default values of pass-to-web options from parsed args. */
export function collectWebAppOptionsFromArgs(
  args: any,
  _schema?: unknown,
): Partial<WebOptionsRecord> {
  const result: Record<string, string | number | boolean> = {}
  const defaults = defaultOptions()
  const flatDefaults = flattenObject(defaults)
  for (const key of PASS_TO_WEB) {
    const value = key.split('.').reduce((o: any, k) => (o == null ? o : o[k]), args as any)
    const def = flatDefaults[key]
    if (value !== undefined && JSON.stringify(value) !== JSON.stringify(def)) {
      result[key] = value as any
    }
  }
  return result as Partial<WebOptionsRecord>
}

/** Build URLSearchParams for non-default pass-to-web options. */
export function buildWebAppURLSearchParamsFromArgs(args: any, _schema?: unknown): URLSearchParams {
  const params = new URLSearchParams()
  const entries = collectWebAppOptionsFromArgs(args)
  for (const [key, val] of Object.entries(entries)) params.append(key, String(val))
  return params
}

/** Parse pass-to-web options from URLSearchParams into a typed record. */
export function parseWebAppOptionsFromSearchParams(
  params: URLSearchParams,
  _schema?: unknown,
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
  const defaults = defaultOptions()
  const flatDefaults = flattenObject(defaults)
  for (const key of PASS_TO_WEB) {
    const value = params.get(key)
    if (value == null) continue
    const def = flatDefaults[key]
    if (typeof def === 'boolean') {
      const b = coerceBoolean(value)
      if (b != null) out[key] = b
    } else if (typeof def === 'number') {
      const n = Number(value)
      if (!Number.isNaN(n)) out[key] = n
    } else {
      out[key] = value
    }
  }
  return out as Partial<WebOptionsRecord>
}
