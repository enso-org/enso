/**
 * Profiling module that supports work with the [Web Performance API](https://developer.mozilla.org/en-US/docs/Web/API/Performance).
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
    const source = callerInfo(2)
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
    const regex = /\((.*):(\d+):(\d+)\)$/
    const caller = e.stack.split('\n')[2 + caller_level]
    if (caller === undefined) {
        return null
    }
    const match = regex.exec(caller)
    if (match === null) {
        return null
    }
    assert(match.length >= 3, 'Regex was expected to produce three matches.')
    return {
        file: match[1],
        line: Number(match[2]),
    }
}

// ==============
// === Labels ===
// ==============

/// Return a string that encodes the given log level and name for a mark that indicates the start of
/// an interval. The label encoded the log level, name and event type.
function startIntervalLabel(metadata: Metadata): string {
    return `${metadata.label} [START]`
}

/// Return a string that encodes the given log level and name for a mark that indicates the end of
/// an interval.
function endIntervalLabel(metadata: Metadata): string {
    return `${metadata.label} [END]`
}

/// Return a string that encodes the given log level and name for display in the developer console
// performance tool..
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
    logLevel: string
    toggledInEnvironment: boolean

    constructor(logLevel: string) {
        this.logLevel = logLevel
        // The value `process.env.PROFILING_LEVEL` is populated through the [environment plugin](https://webpack.js.org/plugins/environment-plugin/)
        // at build time from an environment variable.
        this.toggledInEnvironment = process.env.PROFILING_LEVEL.toLowerCase().includes(
            logLevel.toLowerCase()
        )

        profilerRegistry.push(this)
    }

    logLevelName(): string {
        return this.logLevel.toUpperCase()
    }

    /// Indicate whether this profiler should perform profiling.
    isActive(): boolean {
        // We need to check whether this logger, or a logger that is higher in the hierarchy, is activated.
        // The hierarchy is established by order of instantiation / registration in the `profilerRegistry`.
        const profilerIndex = profilerRegistry.findIndex(item => item === this)
        if (profilerIndex < 0) {
            return false
        }
        for (let i = profilerIndex; i; i--) {
            if (profilerRegistry[i].toggledInEnvironment) {
                return true
            }
        }
        return false
    }

    isLogLevelName(log_level_name: string): boolean {
        return this.logLevelName() == log_level_name.toUpperCase()
    }

    /// Start the profiling of the named interval.
    start(interval_name: string): IntervalHandle {
        if (this.isActive()) {
            const metadata = makeMetadata(this.logLevel, interval_name)
            performance.mark(startIntervalLabel(metadata))
        }
        return new IntervalHandle(interval_name, this)
    }

    _end_with_metadata(metadata: Metadata) {
        const start_label = startIntervalLabel(metadata)
        const end_label = endIntervalLabel(metadata)
        const measurement_label = performanceToolLabel(metadata)
        if (this.isActive()) {
            performance.mark(end_label)
            const options = { detail: metadata, start: start_label, end: end_label }
            try {
                // @ts-ignore TS does not yet support typing for the next gen API of `measure`.
                performance.measure(measurement_label, options)
            } catch (e) {
                console.warn(`Error during profiling: ${e}`)
            }

        }
    }

    /// End the profiling of the named interval.
    end(interval_name: string) {
        const metadata = makeMetadata(this.logLevel, interval_name)
        this._end_with_metadata(metadata)
    }

    /// Profile the execution of the given callable.
    measure(interval_name: string, closure: CallableFunction) {
        const metadata = makeMetadata(this.logLevel, interval_name)
        this.start(interval_name)
        const ret = closure()
        this._end_with_metadata(metadata)
        return ret
    }
}

// ================
// === Decoding ===
// ================

function parseLogLevel(log_level_name: string): Profiler | null {
    for (const profiler of profilerRegistry) {
        if (profiler.isLogLevelName(log_level_name)) {
            return profiler
        }
    }
    console.warn(`Could not parse log level from "${log_level_name}"`)
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
    interval_name: string
    log_level: Profiler
    released_status: ReleasedStatus

    constructor(interval_name: string, log_level: Profiler) {
        this.interval_name = interval_name
        this.log_level = log_level
        this.released_status = new ReleasedStatus()
        this.released_status.released = false
        handle_registry.register(
            this,
            new IntervalFinalizationRegistryEntry(interval_name, this.released_status)
        )
    }

    /// Measure the interval.
    end() {
        const metadata = makeMetadata(this.log_level.logLevel, this.interval_name)
        this.log_level._end_with_metadata(metadata)
        this.released_status.released = true
    }

    /// Release the handle to manually call `end_interval` without emitting a warning.
    release() {
        this.released_status.released = true
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

// We need to ts-ignore here as `FinalizationRegistry` is not recognised by TypeScript as being in
// scope even though it is.
// @ts-ignore
const handle_registry = new FinalizationRegistry(heldValue => {
    if (heldValue instanceof IntervalFinalizationRegistryEntry) {
        if (!heldValue.release_status.released) {
            console.warn(heldValue.interval_name + 'was dropped without a call to `measure`.')
        }
    }
})

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
    readonly logLevel: Profiler
    readonly name: string

    constructor(startTime: number, duration: number, logLevel: Profiler, name: string) {
        this.startTime = startTime
        this.duration = duration
        this.logLevel = logLevel
        this.name = name
    }

    static fromPerformanceEntry(entry: PerformanceEntry): Measurement | null {
        const startTime = entry.startTime
        const duration = entry.duration
        // @ts-ignore TS does not yet support typing for the next gen API of `measure`.
        const detail = entry.detail
        const logLevel = parseLogLevel(detail.profiling_level)
        if (logLevel === null) {
            console.warn(`Could not parse ${detail.profiling_level} as valid profiling level.`)
            return null
        }
        const name = entry.name
        return new Measurement(startTime, duration, logLevel, name)
    }

    endTime(): number {
        return this.startTime + this.duration
    }

    prettyString(): string {
        return `[${this.startTime.toFixed(OUTPUT_PRECISION)},${this.endTime().toFixed(
            OUTPUT_PRECISION
        )}] (${this.duration.toFixed(OUTPUT_PRECISION)}) ${this.name}`
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
        return this.measurements.filter(m => m.logLevel === profiler)
    }

    /**
     * Render the report to the developer console.
     */
    renderToDevConsole() {
        console.groupCollapsed('PerformanceReport')

        for (const profiler of profilerRegistry) {
            console.groupCollapsed(profiler.logLevel)
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

// ==================
// === Public API ===
// ==================

export const section = new Profiler('Section')
export const task = new Profiler('Task')
export const detail = new Profiler('Detail')
export const debug = new Profiler('Debug')
