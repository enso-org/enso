const all_profilers: Profiler[] = []

class Profiler {
    log_level: string

    constructor(logLevel: string) {
        this.log_level = logLevel
        all_profilers.push(this)
    }

    logLevelName(): string {
        return this.log_level.toUpperCase()
    }

    isActive(): boolean {
        /// Note hat this is replaced at build time by-pack see XXX
        return process.env.PROFILING_LEVEL.toLowerCase().includes(this.logLevelName())
    }

    /// Return a string that encodes the given log level and name for a mark that indicates the start of
    /// an interval.
    startIntervalLabel(interval_name: string): string {
        return [this.log_level, interval_name, 'start'].join(MESSAGE_DELIMITER)
    }

    /// Return a string that encodes the given log level and name for a mark that indicates the end of
    /// an interval.
    endIntervalLabel(interval_name: string): string {
        return [this.log_level, interval_name, 'end'].join(MESSAGE_DELIMITER)
    }

    /// Return a string that encodes the given log level and name for a measurement.
    measureIntervalLabel(interval_name: string): string {
        return [this.log_level, interval_name, 'measure'].join(MESSAGE_DELIMITER)
    }

    start(interval_name: string): IntervalHandle {
        if (this.isActive()) {
            performance.mark(this.startIntervalLabel(interval_name))
        }
        return new IntervalHandle(interval_name, this)
    }

    end(interval_name: string) {
        let start_label = this.startIntervalLabel(interval_name)
        let end_label = this.endIntervalLabel(interval_name)
        let measurement_label = this.measureIntervalLabel(interval_name)

        if (this.isActive()) {
            performance.mark(end_label)
            performance.measure(measurement_label, start_label, end_label)
        }
    }

    measure(interval_name: string, closure: CallableFunction) {
        this.start(interval_name)
        const ret = closure()
        this.end(interval_name)
        return ret
    }

    isLogLevelName(log_level_name: string): boolean {
        return this.logLevelName() == log_level_name.toUpperCase()
    }
}

/// Delimiter used to to encode information in the `PerformanceEntry` name.
const MESSAGE_DELIMITER = '//'

function parseLogLevel(log_level_name: string): Profiler | undefined {
    for (const profiler of all_profilers) {
        if (profiler.isLogLevelName(log_level_name)) {
            return profiler
        }
    }
    return undefined
}

function decodeLabel(label: string): [Profiler, string] | undefined {
    let parts = label.split(MESSAGE_DELIMITER)
    try {
        let [log_level_name, interval_name] = [parts[0], parts[1]]
        let profiler = parseLogLevel(log_level_name)
        return [profiler, interval_name]
    } catch (e) {
        return undefined
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
        this.log_level.end(this.interval_name)
        this.released_status.released = true
    }

    /// Release the handle to manually call `end_interval` without emitting a warning.
    release() {
        this.released_status.released = true
    }
}

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

    static fromPerformanceEntry(entry: PerformanceEntry): Measurement | undefined {
        const startTime = entry.startTime
        const duration = entry.duration

        let decodedLabel = decodeLabel(entry.name)
        if (decodedLabel === undefined) {
            console.warn(`Could not decode "${entry.name}" as measurement label.`)
            return
        }
        const [logLevel, name] = decodedLabel

        return new Measurement(startTime, duration, logLevel, name)
    }

    endTime(): number {
        return this.startTime + this.duration
    }

    prettyString(): string {
        return `[${this.startTime.toFixed(2)},${this.endTime().toFixed(
            2
        )}] (${this.duration.toFixed(2)}) ${this.name}`
    }

    prettyPrint() {
        console.log(this.prettyString())
    }
}

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

        for (const profiler of all_profilers) {
            console.groupCollapsed(profiler.log_level)
            this.singleLevelMeasurement(profiler).forEach(m => m.prettyPrint())
            console.groupEnd()
        }

        console.groupEnd()
    }

    toString() {
        let output = ''
        for (const profiler of all_profilers) {
            this.singleLevelMeasurement(profiler).forEach(
                m => (output = output.concat(m.prettyString(), '\n'))
            )
        }
        return output
    }
}

export const section = new Profiler('Section')
export const task = new Profiler('Task')
export const symbol = new Profiler('Symbol')
export const debug = new Profiler('Debug')
