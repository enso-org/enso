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
export type OptionValue = string | boolean | number

export type OptionType = 'string' | 'boolean' | 'number'

/** Configuration parameter. */
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

type OptionsRecord = Record<string, Option<OptionValue>>
type GroupsRecord = Record<string, GroupLike>

interface GroupLike {
    options: OptionsRecord
    groups: GroupsRecord
    setPath(path: string[]): void
    merge<Other extends GroupLike>(other: Other): this & Other
    load(config: unknown): string[]
}

export class Group<Options extends OptionsRecord, Groups extends GroupsRecord> {
    options: Options
    groups: Groups
    constructor(cfg?: { options?: Options; groups?: Groups }) {
        this.options = cfg?.options ?? ({} as Options)
        this.groups = cfg?.groups ?? ({} as Groups)
        for (const [name, option] of Object.entries(this.options)) {
            option.name = name
        }
        for (const [name, group] of Object.entries(this.groups)) {
            group.setPath([name])
        }
    }

    setPath(path: string[]) {
        for (const option of Object.values(this.options)) {
            option.path = path
        }
        for (const [name, group] of Object.entries(this.groups)) {
            group.setPath(path.concat([name]))
        }
    }

    merge<Other extends GroupLike>(other: Other): this & Other {
        const result: GroupLike = new Group()

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
                // TODO warning
            }
            result.options[otherOptionName] = otherOption
        }
        return result as this & Other
    }

    load(config: unknown): string[] {
        let unrecognized: string[] = []
        if (typeof config === 'object' && config != null) {
            for (const [key, value] of Object.entries(config)) {
                if (typeof value === 'string') {
                    const option = this.options[key]
                    if (option == null) {
                        unrecognized.push(key)
                    } else {
                        option.load(value)
                    }
                } else if (typeof value === 'object' && value != null) {
                    const group = this.groups[key]
                    if (group == null) {
                        unrecognized.push(key)
                    } else {
                        unrecognized = unrecognized.concat(group.load(value))
                    }
                } else {
                    unrecognized.push(key)
                }
            }
        } else {
            console.error('TODO')
        }
        return unrecognized
    }
}

/** Application default configuration. Users of this library can extend it with their own
 * options. */
export const options = new Group({
    groups: {
        loader: new Group({
            options: {
                enabled: new Option({
                    default: true,
                    description:
                        'Controls whether the visual loader should be visible on the screen when ' +
                        'downloading and compiling WASM sources. By default, the loader is used only if ' +
                        `the \`entry\` is set to ${DEFAULT_ENTRY_POINT}.`,
                    primary: false,
                }),
                pkgWasmUrl: new Option({
                    default: 'pkg.wasm',
                    description: 'The URL of the WASM pkg file generated by ensogl-pack.',
                    primary: false,
                }),
                pkgJsUrl: new Option({
                    default: 'pkg.js',
                    description: 'The URL of the JS pkg file generated by ensogl-pack.',
                    primary: false,
                }),
                shadersUrl: new Option({
                    default: 'shaders',
                    description: 'The URL of pre-compiled the shaders directory.',
                    primary: false,
                }),
                loaderDownloadToInitRatio: new Option({
                    default: 1.0,
                    description:
                        'The (time needed for WASM download) / (total time including WASM ' +
                        'download and WASM app initialization). In case of small WASM apps, this can be set ' +
                        'to 1.0. In case of bigger WASM apps, it is desired to show the progress bar growing ' +
                        'up to e.g. 70% and leaving the last 30% for WASM app init.',
                    primary: false,
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

        startup: new Group({
            options: {
                entry: new Option({
                    default: DEFAULT_ENTRY_POINT,
                    description:
                        'The application entry point. Use `entry=_` to list available entry points.',
                }),
                theme: new Option({
                    default: 'default',
                    description: 'The EnsoGL theme to be used.',
                }),
            },
        }),

        debug: new Group({
            options: {
                debug: new Option({
                    default: false,
                    description:
                        'Controls whether the application should be run in the debug mode. In this mode all ' +
                        'logs are printed to the console. Otherwise, the logs are hidden unless explicitly ' +
                        'shown by calling `showLogs`. Moreover, EnsoGL extensions are loaded in the debug ' +
                        'mode which may cause additional logs to be printed.',
                }),
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

export type Options = typeof options & GroupLike

// ==============
// === Config ===
// ==============

/** The configuration of the EnsoGL application. The options can be overriden by the user. The
 * implementation automatically casts the values to the correct types. For example, if an option
 * override for type boolean was provided as `'true'`, it will be parsed automatically. Moreover,
 * it is possible to extend the provided option list with custom options. See the `extend` method
 * to learn more. */
export class Config {
    options: Options

    constructor(inputOptions?: Options) {
        if (inputOptions != null) {
            this.options = options.merge(inputOptions)
        } else {
            this.options = options
        }
    }

    /** Resolve the configuration from the provided record list.
     * @returns list of unrecognized parameters. */
    resolve(overrides: (Record<string, Record<string, any>> | undefined)[]): string[] {
        const allOverrides: Record<string, Record<string, any>> = {}
        for (const override of overrides) {
            if (override != null) {
                for (const [group, options] of Object.entries(override)) {
                    const overridesGroup = allOverrides[group] || {}
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

    resolveFromObject(config: Record<string, unknown>): string[] {
        return this.options.load(config)
    }

    /** Finalize the configuration. Set some default options based on the provided values. */
    finalize() {
        if (
            !this.options.groups.loader.options.enabled.setByUser &&
            this.options.groups.startup.options.entry.value !== DEFAULT_ENTRY_POINT
        ) {
            this.options.groups.loader.options.enabled.value = false
        }
    }

    printValueUpdateError(key: string, selfVal: any, otherVal: any) {
        console.error(
            `The provided value for Config.${key} is invalid. Expected boolean, got '${otherVal}'. \
            Using the default value '${selfVal}' instead.`
        )
    }

    strigifiedKeyValueMap(): Record<string, Record<string, any>> {
        // const config: Record<string, Record<string, any>> = {}
        // for (const [group, options] of Object.entries(this.options)) {
        //     const configGroup: Record<string, any> = {}
        //     config[group] = configGroup
        //     for (const [key, option] of Object.entries(options)) {
        //         if (option.value != null) {
        //             configGroup[key] = option.value.toString()
        //         } else {
        //             configGroup[key] = option.value
        //         }
        //     }
        // }
        // return config
        return {}
    }

    print() {
        logger.log(`Resolved config:`, this.strigifiedKeyValueMap())
    }
}
