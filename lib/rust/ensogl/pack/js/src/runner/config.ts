/** @file Configuration options for the application. */

import { logger } from 'runner/log'

export const DEFAULT_ENTRY_POINT = 'ide'

// =============
// === Utils ===
// =============

/** Parses the provided value as boolean. If it was a boolean value, it is left intact. If it was
 * a string 'true', 'false', '1', or '0', it is converted to a boolean value. Otherwise, null is
 * returned. */
// prettier-ignore
function parseBoolean(value: any): boolean | null {
    switch(value) {
        case true: return true
        case false: return false
        case 'true': return true
        case 'false': return false
        case 'enabled': return true
        case 'disabled': return false
        case 'yes': return true
        case 'no': return false
        case '1': return true
        case '0': return false
        default: return null
    }
}

// ==============
// === Option ===
// ==============

/** A valid configuration option value. */
export type OptionValue = string | boolean | number

/** A valid configuration option type. */
export type OptionType = 'string' | 'boolean' | 'number'

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
    setByUser = false
    hidden: boolean
    /** Controls whether this option should be visible by default in the help message. Non-primary
     * options will be displayed on-demand only. */
    primary = true
    constructor(cfg: {
        default: T
        description: string
        defaultDescription?: string
        hidden?: boolean
        primary?: boolean
    }) {
        this.default = cfg.default
        this.value = cfg.default
        this.description = cfg.description
        this.defaultDescription = cfg.defaultDescription ?? null
        this.hidden = cfg.hidden ?? false
        this.primary = cfg.primary ?? true
        if (typeof this.value === 'boolean') {
            this.type = 'boolean'
        } else if (typeof this.value === 'number') {
            this.type = 'number'
        } else {
            this.type = 'string'
        }
    }

    qualifiedName(): string {
        const path = this.path.join('.')
        return `${path}.${this.name}`
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
        logger.error(
            `The provided value for '${this.qualifiedName()}' is invalid. Expected ${this.type}, \
            got '${input}'. Using the default value '${String(this.default)}' instead.`
        )
    }
}

// ==============
// === Options ===
// ==============

export interface StringConfig {
    [key: string]: string | StringConfig
}

/** Record containing options. */
type OptionsRecord = Record<string, Option<OptionValue>>

/** Record containing option groups. */
type GroupsRecord = Record<string, AnyGroup>

/** Options group. The same as `Group` but with elided generic parameters. */
interface AnyGroup {
    options: OptionsRecord
    groups: GroupsRecord
    setPath(path: string[]): void
    merge<Other extends AnyGroup>(other: Other): this & Other
    load(config: StringConfig, stack?: string[]): string[]
    stringify(): StringConfig
    prettyPrint(indent: number): string
}

/** Options group. Allows defining nested options. The class is generic in order to allow TypeScript
 * to infer the types of its children and thus allow accessing them in a type-safe way. */
export class Group<Options extends OptionsRecord, Groups extends GroupsRecord> {
    options: Options = {} as Options
    groups: Groups = {} as Groups
    constructor(cfg?: { options?: Options; groups?: Groups }) {
        const options = cfg?.options
        if (options != null) {
            for (const [name, option] of Object.entries(options)) {
                this.addOption(name, option)
            }
        }
        const groups = cfg?.groups
        if (groups != null) {
            for (const [name, group] of Object.entries(groups)) {
                this.addGroup(name, group)
            }
        }
    }

    addOption(name: string, option: Option<OptionValue>) {
        const existingOption = this.options[name]
        if (existingOption != null) {
            logger.error(`Duplicate config option found '${existingOption.qualifiedName()}'.`)
        }
        const options = this.options as OptionsRecord
        options[name] = option
        option.name = name
    }

    addGroup(name: string, group: AnyGroup) {
        const existingGroup = this.groups[name]
        if (existingGroup != null) {
            existingGroup.merge(group)
        } else {
            const groups = this.groups as GroupsRecord
            groups[name] = group
        }
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

    /** Merge this group definition with another group definition. Returns a deeply merged group. In
     * case the argument will override some options, errors will be logged. */
    merge<Other extends AnyGroup>(other?: Other | null): this & Other {
        if (other == null) {
            return this as this & Other
        } else {
            const result: AnyGroup = new Group()
            Object.assign(result.groups, this.groups)
            for (const [otherGroupName, otherGroup] of Object.entries(other.groups)) {
                const group = result.groups[otherGroupName]
                if (group == null) {
                    result.groups[otherGroupName] = otherGroup
                } else {
                    result.groups[otherGroupName] = group.merge(otherGroup)
                }
            }
            Object.assign(result.options, this.options)
            for (const [otherOptionName, otherOption] of Object.entries(other.options)) {
                const option = result.options[otherOptionName]
                if (option != null) {
                    logger.error(`Duplicate config option found '${option.qualifiedName()}'.`)
                }
                result.options[otherOptionName] = otherOption
            }
            return result as this & Other
        }
    }

    load(config: StringConfig, stack: string[] = []): string[] {
        let unrecognized: string[] = []
        const addUnrecognized = (name: string) => {
            unrecognized.push(stack.concat([name]).join('.'))
        }
        for (const [key, value] of Object.entries(config)) {
            if (typeof value === 'string') {
                const option = this.options[key]
                if (option == null) {
                    addUnrecognized(key)
                } else {
                    option.load(value)
                }
            } else {
                const group = this.groups[key]
                if (group == null) {
                    addUnrecognized(key)
                } else {
                    const subStack = stack.concat([key])
                    unrecognized = unrecognized.concat(group.load(value, subStack))
                }
            }
        }
        return unrecognized
    }

    loadAll(configs: (StringConfig | null | undefined)[]): string[] {
        let unrecognized: string[] = []
        for (const config of configs) {
            if (config != null) {
                unrecognized = unrecognized.concat(this.load(config))
            }
        }
        return unrecognized
    }

    stringify(): StringConfig {
        const config: StringConfig = {}
        const groupsEntries = Object.entries(this.groups)
        if (groupsEntries.length > 0) {
            config.groups = {}
            for (const [name, group] of groupsEntries) {
                config.groups[name] = group.stringify()
            }
        }
        const optionsEntries = Object.entries(this.options)
        if (optionsEntries.length > 0) {
            config.options = {}
            for (const [name, option] of optionsEntries) {
                config.options[name] = option.value.toString()
            }
        }
        return config
    }

    prettyPrint(indent = 0): string {
        // The number is used for sorting in ordering to put options before groups.
        const entries: [string, number, string][] = []
        const optionsEntries = Object.entries(this.options)
        if (optionsEntries.length > 0) {
            for (const [name, option] of optionsEntries) {
                entries.push([name, 0, option.value.toString()])
            }
        }
        const groupsEntries = Object.entries(this.groups)
        if (groupsEntries.length > 0) {
            for (const [name, group] of groupsEntries) {
                entries.push([name, 1, '\n' + group.prettyPrint(indent + 1)])
            }
        }
        entries.sort()
        return entries
            .map(([name, _, value]) => ' '.repeat(2 * indent) + name + ': ' + value)
            .join('\n')
    }
}

// ===============
// === Options ===
// ===============

/** The configuration of the EnsoGL application. The options can be overriden by the user. The
 * implementation automatically casts the values to the correct types. For example, if an option
 * override for type boolean was provided as `'true'`, it will be parsed automatically. Moreover,
 * it is possible to extend the provided option list with custom options. See the `extend` method
 * to learn more. */
export const options = new Group({
    options: {
        debug: new Option({
            default: false,
            description:
                'Controls whether the application should be run in the debug mode. In this mode all ' +
                'logs are printed to the console. Otherwise, the logs are hidden unless explicitly ' +
                'shown by calling `showLogs`. Moreover, EnsoGL extensions are loaded in the debug ' +
                'mode which may cause additional logs to be printed.',
        }),
    },
    groups: {
        loader: new Group({
            options: {
                spinner: new Option({
                    default: true,
                    description:
                        'Controls whether the visual loader should be visible on the screen when ' +
                        'downloading and compiling WASM sources. By default, the loader is used only if ' +
                        `the \`entry\` is set to ${DEFAULT_ENTRY_POINT}.`,
                    primary: false,
                }),
                wasmUrl: new Option({
                    default: 'pkg.wasm',
                    description: 'The URL of the WASM pkg file generated by ensogl-pack.',
                    primary: false,
                }),
                jsUrl: new Option({
                    default: 'pkg.js',
                    description: 'The URL of the JS pkg file generated by ensogl-pack.',
                    primary: false,
                }),
                shadersUrl: new Option({
                    default: 'shaders',
                    description: 'The URL of pre-compiled the shaders directory.',
                    primary: false,
                }),
                downloadToInitRatio: new Option({
                    default: 1.0,
                    description:
                        'The (time needed for WASM download) / (total time including WASM ' +
                        'download and WASM app initialization). In case of small WASM apps, this can be set ' +
                        'to 1.0. In case of bigger WASM apps, it is desired to show the progress bar growing ' +
                        'up to e.g. 70% and leaving the last 30% for WASM app init.',
                    primary: false,
                }),
            },
        }),

        startup: new Group({
            options: {
                entry: new Option({
                    default: DEFAULT_ENTRY_POINT,
                    description:
                        'The application entry point. Use `entry=_` to list available entry points.',
                }),
                maxBeforeMainTimeMs: new Option({
                    default: 300,
                    description:
                        'The maximum time in milliseconds a before main entry point is allowed to run. After ' +
                        'this time, an error will be printed, but the execution will continue.',
                    primary: false,
                }),
            },
        }),

        debug: new Group({
            options: {
                enableSpector: new Option({
                    default: false,
                    description:
                        'Enables SpectorJS. This is a temporary flag to test Spector. It will be removed ' +
                        'after all Spector integration issues are resolved. See: ' +
                        'https://github.com/BabylonJS/Spector.js/issues/252.',
                    primary: false,
                }),
            },
        }),
    },
})

export type Options = typeof options & AnyGroup
