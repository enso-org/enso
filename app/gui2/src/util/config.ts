/** @file Configuration options for an application. */
import CONFIG from '@/config.json' assert { type: 'json' }
import * as version from '@/util/version'

const STRING_TO_BOOLEAN: Record<string, boolean> = {
  true: true,
  false: false,
  enabled: true,
  disabled: false,
  yes: true,
  no: false,
  1: true,
  0: false,
}

/** Parses the provided value as boolean. If it was a boolean value, it is left intact. If it was
 * a string 'true', 'false', '1', or '0', it is converted to a boolean value. Otherwise, null is
 * returned. */
function parseBoolean(value: unknown): boolean | null {
  return typeof value === 'boolean'
    ? value
    : typeof value === 'string'
    ? STRING_TO_BOOLEAN[value] ?? null
    : null
}

export interface StringConfig {
  [key: string]: StringConfig | string
}

/** A valid configuration option value. */
export type OptionValue = string | boolean | number | string[]

export interface RawOption {
  description?: string
  defaultDescription?: string
  value: OptionValue
  valueEval?: string | undefined
  primary?: boolean
}

export interface RawGroup {
  description?: string
  options?: Record<string, RawOption>
  groups?: Record<string, RawGroup>
}

export interface RawConfig {
  options?: Record<string, RawOption>
  groups?: Record<string, RawGroup>
}

export interface Option<T = RawOption> {
  description: string
  defaultDescription: T extends { defaultDescription: string } ? string : string | undefined
  value: T extends { value: infer Value extends OptionValue } ? Value : string
  primary: boolean
}

export interface Group<T = RawGroup> extends Config<T> {
  description: string
}

export interface Config<T = RawConfig> {
  options: T extends { options: infer Options extends object }
    ? { [K in keyof Options]: Option<Options[K]> }
    : {}
  groups: T extends { groups: infer Groups extends object }
    ? { [K in keyof Groups]: Group<Groups[K]> }
    : {}
}

function loadOption<T>(option: T, scope: Record<string, any> = {}): Option<T> {
  const obj = (typeof option !== 'object' || option == null ? {} : option) as Record<
    string,
    unknown
  >
  let value = obj.value ?? ''
  if (typeof obj.valueEval === 'string') {
    const newValue = new Function('scope', 'return ' + obj.valueEval)(scope)
    if (typeof newValue === typeof value) {
      value = newValue
    } else {
      console.error(
        `The value of eval option '${obj.valueEval}' did not resolve to '${typeof value}'.`,
      )
    }
  }
  return {
    description: String(obj.description ?? ''),
    defaultDescription: obj.defaultDescription != null ? String(obj.defaultDescription) : undefined,
    value:
      typeof value === 'string' ||
      typeof value === 'number' ||
      typeof value === 'boolean' ||
      (Array.isArray(value) && value.every((item) => typeof item === 'string'))
        ? value
        : '',
    primary: Boolean(obj.primary ?? true),
  } satisfies Option as any
}

function loadGroup<T>(group: T, scope: Record<string, any> = {}): Group<T> {
  const obj = (typeof group !== 'object' || group == null ? {} : group) as Record<string, unknown>
  return {
    ...loadConfig(group, scope),
    description: String(obj.description ?? ''),
  } satisfies Group as any
}

export function loadConfig<T>(config: T, scope: Record<string, any> = {}): Config<T> {
  if (typeof config !== 'object' || config == null) {
    return { options: {}, groups: {} } satisfies Config as any
  }
  return {
    options:
      'options' in config && typeof config.options === 'object' && config.options != null
        ? Object.fromEntries(
            Object.entries(config.options).map(([k, v]) => [k, loadOption(v, scope)]),
          )
        : {},
    groups:
      'groups' in config && typeof config.groups === 'object' && config.groups != null
        ? Object.fromEntries(
            Object.entries(config.groups).map(([k, v]) => [k, loadGroup(v, scope)]),
          )
        : {},
  } satisfies Config as any
}

interface MergeOptions {
  onUnrecognizedOption?: (path: string[]) => void
  path?: string[]
}

/** Returns a new object if the value was changed, else returns the object unchanged. */
function mergeOption<T extends Option<any>>(option: T, other: unknown, options: MergeOptions = {}) {
  if (other == null) return option
  const otherRaw = other
  if (typeof option.value === 'number') {
    other = Number(other)
  } else if (typeof option.value === 'boolean') {
    other = parseBoolean(other)
  }
  if (
    (typeof other === 'object' && !Array.isArray(other)) ||
    typeof other !== typeof option.value ||
    Number.isNaN(other)
  ) {
    console.error(
      `Invalid value for option '${(options.path ?? []).join('.')}', expected a${
        Array.isArray(option.value) ? 'n array' : ' ' + typeof option.value
      } value', got:`,
      otherRaw,
    )
    return option
  }
  return option.value === other ? option : { ...option, value: other }
}

function mergeGroup<T extends Group<any>>(group: T, other: unknown, options: MergeOptions = {}): T {
  if (typeof other !== 'object' || other == null) return group
  const newGroup = mergeConfig(group, other as StringConfig, options)
  if (newGroup === group) return group
  return { ...newGroup, description: group.description }
}

export function mergeConfig<T extends Config<any>>(
  config: T,
  other: StringConfig,
  options: MergeOptions = {},
): T {
  if (other == null) return config
  let newOptions: Record<string, Option<any>> | undefined
  let newGroups: Record<string, Group<any>> | undefined
  for (const [k, v] of Object.entries(other)) {
    const path = [...(options.path ?? []), k]
    if (k in config.options) {
      newOptions ??= { ...config.options }
      newOptions[k] = mergeOption(newOptions[k]!, v, { ...options, path })
    } else if (k in config.groups) {
      newGroups ??= { ...config.groups }
      newGroups[k] = mergeGroup(newGroups[k]!, v, { ...options, path })
    } else {
      options.onUnrecognizedOption?.(path)
    }
  }
  if (newOptions == null && newGroups == null) return config
  return {
    options: newOptions,
    groups: newOptions,
  } as any
}

export const baseConfig = loadConfig(CONFIG, { Version: version })
export type ApplicationConfig = typeof baseConfig
