/** @file Configuration options for an application. */
import type { HelpInfo, HelpScreenEntry, HelpScreenSection } from '@/components/HelpScreen/types'

export const DEFAULT_ENTRY_POINT = 'ide'

// =============
// === Utils ===
// =============

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

// ==============
// === Option ===
// ==============

/** A valid configuration option value. */
export type OptionValue = string | boolean | number | string[]

/** A valid configuration option type. */
export type OptionType = 'string' | 'boolean' | 'number' | 'array'

/** Non-generic version of `Option`. */
export type AnyOption = Option<OptionValue>

/** Plain object version of `Option`. Used mainly when loading options from a JSON file. */
export interface OptionObject<T> {
  value: T
  valueEval?: string
  description: string
  defaultDescription?: string | null
  hidden?: boolean
  primary?: boolean
  passToWebApplication?: boolean
}

/** Non-generic version of `OptionObject`. */
export type AnyOptionObject = OptionObject<OptionValue>

/** Configuration option. */
export class Option<T> {
  name = 'unnamed'
  path: string[] = []
  default: T
  value: T
  type: OptionType
  description: string
  /** The description of the default argument that should be shown to the user. For example,
   * it can be set to `'true' on macOS and 'false' otherwise` to better explain mechanics for the
   * default value. */
  defaultDescription: null | string = null
  /** If set to false, the option will not be passed to the web application. This is useful when
   * creating meta configuration options, that control some behavior, like the window style, but
   * are not designed to control the web app behavior. */
  passToWebApplication = true
  setByUser = false
  hidden: boolean
  /** Controls whether this option should be visible by default in the help message. Non-primary
   * options will be displayed on-demand only. */
  primary = true

  constructor(cfg: OptionObject<T>) {
    this.default = cfg.value
    this.value = cfg.value
    this.description = cfg.description
    this.defaultDescription = cfg.defaultDescription ?? null
    this.hidden = cfg.hidden ?? false
    this.primary = cfg.primary ?? true
    this.passToWebApplication = cfg.passToWebApplication ?? true
    if (typeof this.value === 'boolean') {
      this.type = 'boolean'
    } else if (typeof this.value === 'number') {
      this.type = 'number'
    } else if (Array.isArray(this.value)) {
      this.type = 'array'
    } else {
      this.type = 'string'
    }
  }

  /** Return a copy of this option. */
  clone(): Option<T> {
    return Object.assign(new Option<T>(this), this)
  }

  /** Names of all parent groups and name of this option intercalated with dots. */
  qualifiedName(): string {
    return this.path.concat([this.name]).join('.')
  }

  load(input: string) {
    if (typeof this.value === 'boolean') {
      const newVal = parseBoolean(input)
      if (newVal == null) {
        this.printValueUpdateError(input)
      } else {
        this.value = newVal as T
        this.setByUser = true
      }
    } else if (typeof this.value == 'number') {
      const newVal = Number(input)
      if (isNaN(newVal)) {
        this.printValueUpdateError(input)
      } else {
        this.value = newVal as T
        this.setByUser = true
      }
    } else {
      this.value = String(input) as T
      this.setByUser = true
    }
  }

  printValueUpdateError(input: string) {
    console.error(
      `The provided value for '${this.qualifiedName()}' is invalid. Expected ${this.type}, ` +
        `got '${input}'. Using the default value '${String(this.default)}' instead.`,
    )
  }
}

// =============
// === Group ===
// =============

export interface StringConfig {
  [key: string]: string | StringConfig
}

/** Record containing options. */
type OptionsRecord = Record<string, AnyOption>

/** Record containing option groups. */
type GroupsRecord = Record<string, AnyGroup>

/** Plain object representation of `Group`. Used mainly to load groups from JSON files. */
export interface GroupObject {
  description?: string
  options?: Record<string, AnyOptionObject>
  groups?: Record<string, GroupObject>
}

/** Options group. The same as `Group` but with elided generic parameters. */
export interface AnyGroup {
  name: string
  description: string
  options: OptionsRecord
  groups: GroupsRecord
  setPath(path: string[]): void
  clone(): this
  load(config: StringConfig, stack?: string[]): string[]
  optionsRecursive(): AnyOption[]
}

/** Options group. Allows defining nested options. The class is generic in order to allow TypeScript
 * to infer the types of its children and thus allow accessing them in a type-safe way. */
export class Group<Options extends OptionsRecord, Groups extends GroupsRecord> {
  name = 'unnamed'
  description
  options = {} as Options
  groups = {} as Groups

  constructor(cfg?: {
    description?: string | undefined
    options?: Options | undefined
    groups?: Groups | undefined
  }) {
    this.description = cfg?.description ?? 'No description.'
    for (const [name, option] of Object.entries(cfg?.options ?? {})) {
      this.addOption(name, option)
    }
    for (const [name, group] of Object.entries(cfg?.groups ?? {})) {
      this.addGroup(name, group)
    }
  }

  addOption(name: string, option: AnyOption) {
    const existingOption = this.options[name]
    if (existingOption != null) {
      console.error(`Duplicate config option found '${existingOption.qualifiedName()}'.`)
    }
    const options = this.options as OptionsRecord
    options[name] = option
    option.name = name
  }

  addGroup(name: string, group: AnyGroup) {
    const groups = this.groups as GroupsRecord
    groups[name] = group
    group.name = name
    group.setPath([name])
  }

  /** Set the path of this group. If this group was placed in another group, the path will contain
   * all the parent group names. */
  setPath(path: string[]) {
    for (const option of Object.values(this.options)) {
      option.path = path
    }
    for (const [name, group] of Object.entries(this.groups)) {
      group.setPath(path.concat([name]))
    }
  }

  /** Return a deep copy of this group definition. */
  clone(): this {
    const result: AnyGroup = new Group()
    for (const [name, group] of Object.entries(this.groups)) {
      result.groups[name] = group.clone()
    }
    for (const [name, option] of Object.entries(this.options)) {
      result.options[name] = option.clone()
    }
    return result as this
  }

  load(config: StringConfig, stack: string[] = []): [...unrecognizedOptions: string[]] {
    const unrecognized: string[] = []
    const addUnrecognized = (name: string) => {
      unrecognized.push([...stack, name].join('.'))
    }
    for (const [key, value] of Object.entries(config)) {
      if (typeof value === 'string') {
        const option = this.options[key]
        if (!option) {
          addUnrecognized(key)
        } else {
          option.load(value)
        }
      } else {
        const group = this.groups[key]
        if (!group) {
          addUnrecognized(key)
        } else {
          unrecognized.push(...group.load(value, [...stack, key]))
        }
      }
    }
    return unrecognized
  }

  loadAllAndReturnHelpInfo(configs: (StringConfig | null | undefined)[]): HelpInfo | undefined {
    const unrecognized = configs.flatMap((config) => (config != null ? this.load(config) : []))
    if (unrecognized.length === 0) return
    console.error(`Unrecognized configuration parameters: ${unrecognized.join(', ')}.`)
    return this.helpInfo(unrecognized)
  }

  optionsRecursive(): AnyOption[] {
    return [
      Object.values(this.options),
      ...Object.values(this.groups).map((group) => group.optionsRecursive()),
    ].flat()
  }

  /** Error details returned when invalid options are encountered. */
  helpInfo(unknownOptions?: string[]): HelpInfo {
    console.log('Showing config options help screen.')
    let msg = ''
    if (unknownOptions) {
      const optionLabel = unknownOptions.length > 1 ? 'options' : 'option'
      msg = `Unknown config ${optionLabel}: ${unknownOptions.map((t) => `'${t}'`).join(', ')}. `
    }
    const sectionsData: [string, string, HelpScreenEntry[]][] = Object.entries(this.groups).map(
      ([groupName, group]) => {
        const groupOptions = group.optionsRecursive()
        const entriesData: [string, string, string][] = groupOptions.map((opt) => [
          opt.qualifiedName(),
          opt.description,
          String(opt.default),
        ])
        entriesData.sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0))
        const entries = entriesData.map(
          ([name, description, def]): HelpScreenEntry => ({ name, values: [description, def] }),
        )
        const option = this.options[groupName]
        if (option != null) {
          entries.unshift({ name: groupName, values: [option.description, String(option.default)] })
        }
        const name =
          groupName.charAt(0).toUpperCase() +
          groupName.slice(1).replace(/([A-Z])/g, ' $1') +
          ' Options'
        const description = group.description
        return [name, description, entries]
      },
    )
    sectionsData.sort()
    const sections = sectionsData.map(
      ([name, description, entries]): HelpScreenSection => ({ name, description, entries }),
    )

    const rootEntries = Object.entries(this.options).flatMap(
      ([optionName, option]): HelpScreenEntry[] => {
        if (optionName in this.groups) return []
        return [{ name: optionName, values: [option.description, String(option.default)] }]
      },
    )
    if (rootEntries.length > 0) {
      const name = 'Other Options'
      sections.push({ name, entries: rootEntries })
    }

    const title = msg + 'Available options:'
    const headers = ['Name', 'Description', 'Default']
    return { title, headers, sections }
  }
}

// ===============
// === Options ===
// ===============

/** Type-level conversion of `OptionObject` to `Option`. */
type ToOption<T extends AnyOptionObject> = Option<T['value']>

/** Type-level conversion of `GroupObject` to `Group`. */
type ToGroup<T extends GroupObject> = Group<
  { [K in keyof T['options']]: ToOption<NonNullable<T['options']>[K]> },
  { [K in keyof T['groups']]: ToGroup<NonNullable<T['groups']>[K]> }
>

/** Convert the plain group object to a `Group` object instance. */
export function objectToGroup<T extends GroupObject>(obj: T, scope: object = {}): ToGroup<T> {
  const options: Record<string, AnyOption> = {}
  const groups: Record<string, AnyGroup> = {}
  if (obj.options) {
    for (const [name, option] of Object.entries(obj.options)) {
      options[name] = objectToOption(option, scope)
    }
  }
  if (obj.groups) {
    for (const [name, group] of Object.entries(obj.groups)) {
      groups[name] = objectToGroup(group, scope)
    }
  }
  const description = obj.description
  return new Group({ description, options, groups }) as ToGroup<T>
}

/** Convert the plain option object to an `Option` object instance. */
export function objectToOption<T extends AnyOptionObject>(obj: T, scope: object): ToOption<T> {
  const code = obj.valueEval
  if (code != null) {
    const value: unknown = new Function('scope', 'return ' + code)(scope)
    const expectedType = typeof obj.value
    if (typeof value === typeof obj.value) {
      obj.value = value as OptionValue
    } else {
      console.error(`The value of eval option '${code}' did not resolve to '${expectedType}'.`)
    }
  }
  return new Option(obj)
}
