/**
 * Profiling module that supports work with the [Web Performance API]
 * (https://developer.mozilla.org/en-US/docs/Web/API/Performance).
 *
 * Allows structured profiling of code with a hierarchical API.
 *
 * Example usage
 * -------------
 * ```
 * // Manually start and end the measurement.
 * const profilingHandle = profiling.task.start(doSomething')
 * doSomething()
 * profilingHandle.end()
 *
 * // Or use the `measure` method.
 * profiling::task::measure("doSomething", doSomething);
 *
 * ```
 *
 * Note that this API and encoding formats for messages are synced with the rust equivalent in
 * `lib/rust/profiling/src/lib.rs`.
 */
import * as assert from 'assert'

// =========================
// === Profiler Registry ===
// =========================

/// List of all instantiated profilers.
const profilerRegistry: Profiler[] = []

// ================
// === Metadata ===
// ================

type SourceLocation = {
    file: String
    line: number
}

type Metadata = {
    source: SourceLocation
    profiling_level: string
    label: string
}

/**
 * Create a Metadata object that gets populated with the `SourceLocation` of the caller of the
 * caller of this methods.
 */
function makeMetadata(profiling_level: string, label: string): Metadata {
    // We need to move up the stack by 2 levels to end up at the call of the caller of this
    // function.
    const callerLevel = 2
    const source = callerInfo(callerLevel)
    return {
        source,
        profiling_level,
        label,
    }
}

/**
 * Return the `SourceLocation` of this method call. By supplying the `caller_level` argument, the
 * `SourceLocation` will be shifted up the call stack by that number of levels.
 *
 */
function callerInfo(caller_level = 0): SourceLocation | null {
    const e = new Error()
    const regex = /\((?<filePath>.*):(?<line>\d+):(?<column>\d+)\)$/
    const caller = e.stack.split('\n')[2 + caller_level]
    if (!caller) {
        return null
    }
    const match = regex.exec(caller)
    if (!match) {
        return null
    }
    return {
        file: match.groups['filePath'],
        line: Number(match.groups['line']),
    }
}

// ==============
// === Labels ===
// ==============

// Return a string that encodes the given profiler level and name for a mark that indicates the
// start of an interval. The label encoded the profiler level, name and event type.
function startIntervalLabel(metadata: Metadata): string {
    return `${metadata.label} [START]`
}

// Return a string that encodes the given profiler level and name for a mark that indicates the
// end of an interval.
function endIntervalLabel(metadata: Metadata): string {
    return `${metadata.label} [END]`
}

// Return a string that encodes the given profiler level and name for display in the developer
// console performance tool.
function performanceToolLabel(metadata: Metadata): string {
    return `${metadata.label} (${metadata.source.file}:${metadata.source.line})`
}

// ================
// === Profiler ===
// ================

/**
 * Class that exposes `start`, `end` and `measure` methods, as well as some utility methods for
 * working with the profiler.
 */
class Profiler {
    profileLevel: string
    toggledInEnvironment: boolean

    constructor(profilerLevel: string) {
        this.profileLevel = profilerLevel
        // The value `process.env.PROFILING_LEVEL` is populated through the [environment plugin]
        // (https://webpack.js.org/plugins/environment-plugin/) at build time from an environment
        // variable.
        this.toggledInEnvironment = process.env.PROFILING_LEVEL.toLowerCase().includes(
            profilerLevel.toLowerCase()
        )

        profilerRegistry.push(this)
    }

    profileLevelName(): string {
        return this.profileLevel.toUpperCase()
    }

    // Indicate whether this profiler should perform profiling.
    isActive(): boolean {
        // We need to check whether this profiler, or a profiler that is higher in the hierarchy, is
        // activated. The hierarchy is established by order of instantiation / registration in the
        // `profilerRegistry`.
        const profilerIndex = profilerRegistry.findIndex(item => item === this)
        if (profilerIndex < 0) {
            const profilerLevelName = this.profileLevelName()
            const warning = `Looked for invalid profiler in profilerRegistry: ${profilerLevelName}`
            console.warn(warning)
            return false
        }
        for (let i = profilerIndex; i < profilerRegistry.length; i++) {
            if (profilerRegistry[i].toggledInEnvironment) {
                return true
            }
        }
        return false
    }

    isProfileLevelName(profilerLevelName: string): boolean {
        return this.profileLevelName() == profilerLevelName.toUpperCase()
    }

    // Start the profiling of the named interval.
    start(intervalName: string): IntervalHandle {
        if (this.isActive()) {
            const metadata = makeMetadata(this.profileLevel, intervalName)
            performance.mark(startIntervalLabel(metadata))
        }
        return new IntervalHandle(intervalName, this)
    }

    _end_with_metadata(metadata: Metadata) {
        const start_label = startIntervalLabel(metadata)
        const end_label = endIntervalLabel(metadata)
        const measurement_label = performanceToolLabel(metadata)
        if (this.isActive()) {
            performance.mark(end_label)
            const options = { detail: metadata, start: start_label, end: end_label }
            try {
                // @ts-ignore TS does not yet support typing for the User Timing Level 3
                // Specification.
                performance.measure(measurement_label, options)
            } catch (e) {
                console.warn(`Error during profiling: ${e}`)
            }
        }
    }

    /// End the profiling of the named interval.
    end(interval_name: string) {
        const metadata = makeMetadata(this.profileLevel, interval_name)
        this._end_with_metadata(metadata)
    }

    /// Profile the execution of the given callable.
    measure(interval_name: string, closure: CallableFunction) {
        const metadata = makeMetadata(this.profileLevel, interval_name)
        this.start(interval_name).release()
        const ret = closure()
        this._end_with_metadata(metadata)
        return ret
    }
}

// ================
// === Decoding ===
// ================

function parseProfilerLevel(profilerLevelName: string): Profiler | null {
    for (const profiler of profilerRegistry) {
        if (profiler.isProfileLevelName(profilerLevelName)) {
            return profiler
        }
    }
    console.warn(`Could not parse profiler level from "${profilerLevelName}"`)
    return null
}

// ======================
// === IntervalHandle ===
// ======================

/**
 * Handle that allows the setting of an `end` marker for a profiling interval that was started via
 * the `start` function.
 */
class IntervalHandle {
    intervalName: string
    profilerLevel: Profiler
    releasedStatus: ReleasedStatus

    constructor(intervalName: string, profilerLevel: Profiler) {
        this.intervalName = intervalName
        this.profilerLevel = profilerLevel
        this.releasedStatus = new ReleasedStatus()
        this.releasedStatus.released = false
        let entry = new IntervalFinalizationRegistryEntry(intervalName, this.releasedStatus)
        if (handleRegistry) {
            handleRegistry.register(this, entry)
        }
    }

    /// Measure the interval.
    end() {
        const metadata = makeMetadata(this.profilerLevel.profileLevel, this.intervalName)
        this.profilerLevel._end_with_metadata(metadata)
        this.releasedStatus.released = true
    }

    /// Release the handle to manually call `end_interval` without emitting a warning.
    release() {
        this.releasedStatus.released = true
    }
}

/**
 * Class that contains the information added to the `FinalizationRegistry` to decide whether to emit
 * a warning when a `IntervalHandle` is garbage collected.
 */
class IntervalFinalizationRegistryEntry {
    interval_name: string
    release_status: ReleasedStatus

    constructor(interval_name: string, release_status: ReleasedStatus) {
        this.interval_name = interval_name
        this.release_status = release_status
    }
}

/** Class that contains information about the release status of an `IntervalHandle`. An
 * `IntervalHandle` handle is released by calling `end` or `release`. If it is garbage collected
 * without being released, we want to emit a warning. In order to be able to keep track of the
 * release status of an `IntervalHandle` we need to keep the status boolean in a separate class. */
class ReleasedStatus {
    released: boolean

    constructor() {
        this.released = false
    }
}

let finalizationRegistry
try {
    // We need to ts-ignore here as `FinalizationRegistry` is not recognised by TypeScript as being in
    // scope even though it is.
    // @ts-ignore
    finalizationRegistry = new FinalizationRegistry(heldValue => {
        if (heldValue instanceof IntervalFinalizationRegistryEntry) {
            if (!heldValue.release_status.released) {
                const interval_name = heldValue.interval_name
                console.warn(`${interval_name} was dropped without explicit end of measurement.`)
            }
        }
    })
} catch (e) {
    console.warn(`Initialisation of FinalizationRegistry registry failed because of ${e}.`)
}

const handleRegistry = finalizationRegistry

if (!finalizationRegistry) {
    console.error(
        'Browser does not support use of `FinalizationRegistry`. ' +
            'Profiling might not work correctly if handles are dropped.'
    )
}

// ===================
// === Measurement ===
// ===================

// Number of decimals to show in floating point output.
const OUTPUT_PRECISION = 2

/**
 * Profiling measurement that contains the profiling information about a measured time interval.
 */
class Measurement {
    readonly startTime: number
    readonly duration: number
    readonly profilerLevel: Profiler
    readonly name: string

    constructor(startTime: number, duration: number, profilerLevel: Profiler, name: string) {
        this.startTime = startTime
        this.duration = duration
        this.profilerLevel = profilerLevel
        this.name = name
    }

    static fromPerformanceEntry(entry: PerformanceEntry): Measurement | null {
        const startTime = entry.startTime
        const duration = entry.duration
        // @ts-ignore TS does not yet support typing for the User Timing Level 3
        // Specification.
        const detail = entry.detail
        const profilerLevel = parseProfilerLevel(detail.profiling_level)
        if (profilerLevel === null) {
            const profiling_level = detail.profiling_level
            console.warn(`Could not parse ${profiling_level} as valid profiling level.`)
            return null
        }
        const name = entry.name
        return new Measurement(startTime, duration, profilerLevel, name)
    }

    endTime(): number {
        return this.startTime + this.duration
    }

    prettyString(): string {
        const start = this.startTime.toFixed(OUTPUT_PRECISION)
        const end = this.endTime().toFixed(OUTPUT_PRECISION)
        const duration = this.duration.toFixed(OUTPUT_PRECISION)
        const name = this.name
        return `[${start},${end}] (${duration}) ${name}`
    }

    prettyPrint() {
        console.log(this.prettyString())
    }
}

// ==============
// === Report ===
// ==============

/**
 * Report of all measurements taken during the current session.
 */
export class Report {
    measurements: Measurement[]

    constructor() {
        const entries = performance.getEntriesByType('measure')
        this.measurements = entries.map(entry => Measurement.fromPerformanceEntry(entry))
    }

    singleLevelMeasurement(profiler: Profiler) {
        return this.measurements.filter(m => m.profilerLevel === profiler)
    }

    /**
     * Render the report to the developer console.
     */
    renderToDevConsole() {
        console.groupCollapsed('PerformanceReport')

        for (const profiler of profilerRegistry) {
            console.groupCollapsed(profiler.profileLevel)
            this.singleLevelMeasurement(profiler).forEach(m => m.prettyPrint())
            console.groupEnd()
        }

        console.groupEnd()
    }

    toString() {
        let output = ''
        for (const profiler of profilerRegistry) {
            this.singleLevelMeasurement(profiler).forEach(
                m => (output = output.concat(m.prettyString(), '\n'))
            )
        }
        return output
    }
}

// ================================
// === Public API for Profilers ===
// ================================

/// These need to be defined manually for type information generated by TS to be correct.
/// They need to kept in sync with the `./profilers.json` and the Rust API in
// `lib/rust/profiling/src/lib.rs`
export const section = new Profiler('Section')
export const task = new Profiler('Task')
export const detail = new Profiler('Detail')
export const debug = new Profiler('Debug')
