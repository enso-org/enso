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

/** A valid parameter value. */
export type OptionValue = string | boolean | number | (string | null)

export type OptionType = 'string' | 'boolean' | 'number'

/** Configuration parameter. */
export class Option {
    name = 'uninitialized'
    group = 'uninitialized'
    default: OptionValue
    value: OptionValue
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
        default: OptionValue
        type: OptionType
        description: string
        defaultDescription?: string
        hidden?: boolean
        primary?: boolean
    }) {
        this.default = cfg.default
        this.value = cfg.default
        this.description = cfg.description
        this.defaultDescription = cfg.defaultDescription ?? null
        this.type = cfg.type
        this.hidden = cfg.hidden ?? false
        this.primary = cfg.primary ?? true
    }

    qualifiedName(): string {
        return this.group && this.group != this.name ? `${this.group}.${this.name}` : this.name
    }

    load(input: string) {
        if (this.type === 'boolean') {
            const newVal = parseBoolean(input)
            if (newVal == null) {
                this.printValueUpdateError(input)
            } else {
                this.value = newVal
                this.setByUser = true
            }
        } else if (this.type == 'number') {
            const newVal = Number(input)
            if (isNaN(newVal)) {
                this.printValueUpdateError(input)
            } else {
                this.value = newVal
                this.setByUser = true
            }
        } else {
            this.value = String(input)
            this.setByUser = true
        }
    }

    printValueUpdateError(value: any) {
        console.error(
            `The provided value for '${this.qualifiedName()}' is invalid. Expected ${this.type}, \
            got '${value}'. Using the default value '${String(this.default)}' instead.`
        )
    }
}

// ===============
// === Options ===
// ===============

interface GroupLike {
    description?: string
    options?: Record<string, Option>
    groups?: Record<string, GroupLike>
}

class Group {
    description: string
    options: Record<string, Option>
    groups: Record<string, Group>
    constructor(cfg: {
        description: string
        options?: Record<string, Option>
        groups?: Record<string, Group>
    }) {
        this.description = cfg.description
        this.options = cfg.options ?? {}
        this.groups = cfg.groups ?? {}
    }

    read(path: string): null | Option {}

    read_from_path_array(path: string[]): null | Option {
        const head = path[0]
        if (head == null) {
            return null
        } else if (path.length == 1) {
            return this.options[path[0]] ?? null
        } else {
            const group = this.groups[path[0]]
            if (group == null) {
                return null
            } else {
                return group.read_from_path_array(path.slice(1))
            }
        }
    }

    load(config: unknown): void {
        if (typeof config == 'object' && config != null) {
            for (const [name, value] of Object.entries(config)) {
                if (typeof value == 'string') {
                    const option = this.options[name]
                    if (option != null) {
                        option.load(value)
                    } else {
                        console.error('TODO')
                    }
                }
            }
        } else {
            console.error('TODO')
        }
    }

    static fromGroupLike(input: GroupLike): Group {
        let description = 'No description provided.'
        if (input.description == null) {
            logger.warn(`Group without description.`)
        } else {
            description = input.description
        }
        const options = input.options ?? {}
        const groups: Record<string, Group> = {}
        if (input.groups != null) {
            for (const [name, group] of Object.entries(input.groups)) {
                groups[name] = Group.fromGroupLike(group)
            }
        }
        return new Group({ description, options, groups })
    }

    merge(other?: GroupLike): Group {
        if (other == null) {
            return this
        } else {
            const options = { ...this.options }
            if (other.options != null) {
                for (const [name, option] of Object.entries(other.options)) {
                    const existingOption = options[name]
                    if (existingOption) {
                        logger.warn(`Duplicate option found: '${existingOption.qualifiedName()}'.`)
                    }
                    options[name] = option
                }
            }
            const groups = { ...this.groups }
            if (other.groups != null) {
                for (const [name, group] of Object.entries(other.groups)) {
                    const existingGroup = groups[name]
                    if (existingGroup) {
                        groups[name] = existingGroup.merge(group)
                    } else {
                        groups[name] = Group.fromGroupLike(group)
                    }
                }
            }
            // FIXME: check for double description
            return new Group({
                description: this.description,
                options,
                groups,
            })
        }
    }
}

/** Application default configuration. Users of this library can extend it with their own options. */
export const options = new Group({
    description: 'Application configuration.',
    groups: {
        loader: new Group({
            description: 'JS and WASM Loader.',
            options: {
                enabled: new Option({
                    type: 'boolean',
                    default: true,
                    description:
                        'Controls whether the visual loader should be visible on the screen when ' +
                        'downloading and compiling WASM sources. By default, the loader is used only if ' +
                        `the \`entry\` is set to ${DEFAULT_ENTRY_POINT}.`,
                    primary: false,
                }),
                pkgWasmUrl: new Option({
                    type: 'string',
                    default: 'pkg.wasm',
                    description: 'The URL of the WASM pkg file generated by ensogl-pack.',
                    primary: false,
                }),
                pkgJsUrl: new Option({
                    type: 'string',
                    default: 'pkg.js',
                    description: 'The URL of the JS pkg file generated by ensogl-pack.',
                    primary: false,
                }),
                shadersUrl: new Option({
                    type: 'string',
                    default: 'shaders',
                    description: 'The URL of pre-compiled the shaders directory.',
                    primary: false,
                }),
                loaderDownloadToInitRatio: new Option({
                    type: 'number',
                    default: 1.0,
                    description:
                        'The (time needed for WASM download) / (total time including WASM ' +
                        'download and WASM app initialization). In case of small WASM apps, this can be set ' +
                        'to 1.0. In case of bigger WASM apps, it is desired to show the progress bar growing ' +
                        'up to e.g. 70% and leaving the last 30% for WASM app init.',
                    primary: false,
                }),
                maxBeforeMainTimeMs: new Option({
                    type: 'number',
                    default: 300,
                    description:
                        'The maximum time in milliseconds a before main entry point is allowed to run. After ' +
                        'this time, an error will be printed, but the execution will continue.',
                    primary: false,
                }),
            },
        }),
        // === Application Startup Options ===

        startup: new Group({
            description: 'bar',
            options: {
                entry: new Option({
                    type: 'string',
                    default: DEFAULT_ENTRY_POINT,
                    description:
                        'The application entry point. Use `entry=_` to list available entry points.',
                }),
                theme: new Option({
                    type: 'string',
                    default: 'default',
                    description: 'The EnsoGL theme to be used.',
                }),
            },
        }),

        debug: new Group({
            description: 'baz',
            options: {
                debug: new Option({
                    type: 'boolean',
                    default: false,
                    description:
                        'Controls whether the application should be run in the debug mode. In this mode all ' +
                        'logs are printed to the console. Otherwise, the logs are hidden unless explicitly ' +
                        'shown by calling `showLogs`. Moreover, EnsoGL extensions are loaded in the debug ' +
                        'mode which may cause additional logs to be printed.',
                }),
                enableSpector: new Option({
                    type: 'boolean',
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

// export function mergeOptions<T1 extends Options, T2 extends Options>(
//     opts1: T1,
//     opts2: T2
// ): T1 & T2 {
//     const result: ExternalOptions = {}

//     for (const [group, options] of Object.entries(opts1)) {
//         result[group] = options
//     }
//     for (const [group, options] of Object.entries(opts2)) {
//         if (result[group]) {
//             result[group] = { ...result[group], ...options }
//         } else {
//             result[group] = options
//         }
//     }
//     return result as T1 & T2
// }

// ==============
// === Config ===
// ==============

/** The configuration of the EnsoGL application. The options can be overriden by the user. The
 * implementation automatically casts the values to the correct types. For example, if an option
 * override for type boolean was provided as `'true'`, it will be parsed automatically. Moreover,
 * it is possible to extend the provided option list with custom options. See the `extend` method
 * to learn more. */
export class Config {
    options: Group

    constructor(inputOptions?: GroupLike) {
        this.options = options.merge(inputOptions)
    }

    /** Resolve the configuration from the provided record list.
     * @returns list of unrecognized parameters. */
    resolve(overrides: (Record<string, Record<string, any>> | undefined)[]): null | string[] {
        const allOverrides: Record<string, Record<string, any>> = {}
        for (const override of overrides) {
            if (override != null) {
                for (const [group, options] of Object.entries(override)) {
                    const overridesGroup = allOverrides[group] ?? {}
                    allOverrides[group] = Object.assign(overridesGroup, options)
                }
            }
        }
        const unrecognizedParams = this.resolveFromObject(allOverrides)
        this.finalize()
        return unrecognizedParams
    }

    /** Resolve the configuration from the provided record.
     * @returns list of unrecognized parameters. */
    resolveFromObject(other: unknown): null | string[] {
        this.options.load(other)
        return null
    }

    /** Finalize the configuration. Set some default options based on the provided values. */
    finalize() {
        if (
            !this.options.loader.enabled.setByUser &&
            this.options.startup.entry.value !== DEFAULT_ENTRY_POINT
        ) {
            this.options.loader.enabled.value = false
        }
    }

    printValueUpdateError(key: string, selfVal: any, otherVal: any) {
        console.error(
            `The provided value for Config.${key} is invalid. Expected boolean, got '${otherVal}'. \
            Using the default value '${selfVal}' instead.`
        )
    }

    strigifiedKeyValueMap(): Record<string, Record<string, any>> {
        const config: Record<string, Record<string, any>> = {}
        for (const [group, options] of Object.entries(this.options)) {
            const configGroup: Record<string, any> = {}
            config[group] = configGroup
            for (const [key, option] of Object.entries(options)) {
                if (option.value != null) {
                    configGroup[key] = option.value.toString()
                } else {
                    configGroup[key] = option.value
                }
            }
        }
        return config
    }

    print() {
        logger.log(`Resolved config:`, this.strigifiedKeyValueMap())
    }
}
