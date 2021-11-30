export enum LogLevel {
    Section = 'SECTION',
    Task = 'TASK',
    Detail = 'DETAIL',
    Debug = 'DEBUG',
}

/// Delimiter used to to encode information in the `PerformanceEntry` name.
const MESSAGE_DELIMITER = '//'

const ENABLE_SECTION_PROFILING = process.env.PROFILING_LEVEL.toLowerCase().includes('section')
const ENABLE_TASK_PROFILING = process.env.PROFILING_LEVEL.toLowerCase().includes('task')
const ENABLE_DETAIL_PROFILING = process.env.PROFILING_LEVEL.toLowerCase().includes('detail')
const ENABLE_DEBUG_PROFILING = process.env.PROFILING_LEVEL.toLowerCase().includes('debug')

/// Check whether the given log level should perform any logging activity.
function log_level_is_active(log_level: LogLevel): boolean {
    switch (log_level) {
        case LogLevel.Section:
            return ENABLE_SECTION_PROFILING
        case LogLevel.Task:
            return ENABLE_TASK_PROFILING
        case LogLevel.Detail:
            return ENABLE_DETAIL_PROFILING
        case LogLevel.Debug:
            return ENABLE_DEBUG_PROFILING
    }
}

/// Return a string that encodes the given log level and name for a mark that indicates the start of
/// an interval.
function start_interval_label(log_level: LogLevel, interval_name: string): string {
    return log_level.toString() + MESSAGE_DELIMITER + interval_name + MESSAGE_DELIMITER + 'start'
}

/// Return a string that encodes the given log level and name for a mark that indicates the end of
/// an interval.
function end_interval_label(log_level: LogLevel, interval_name: string): string {
    return log_level.toString() + MESSAGE_DELIMITER + interval_name + MESSAGE_DELIMITER + 'end'
}

/// Return a string that encodes the given log level and name for a measurement.
function measure_interval_label(log_level: LogLevel, interval_name: string): string {
    return log_level.toString() + MESSAGE_DELIMITER + interval_name + MESSAGE_DELIMITER + 'measure'
}

function parse_log_level(log_level_name: string): LogLevel {
    switch (log_level_name) {
        case LogLevel.Section.toString():
            return LogLevel.Section
        case LogLevel.Task.toString():
            return LogLevel.Task
        case LogLevel.Detail.toString():
            return LogLevel.Detail
        case LogLevel.Debug.toString():
            return LogLevel.Debug
    }
}

function decode_label(label: string): [LogLevel, string] {
    let parts = label.split(MESSAGE_DELIMITER)
    let interval_name = parts[1]
    let log_level = parse_log_level(parts[0])
    return [log_level, interval_name]
}

/// Class that enables indirection to the status boolean that indicates whether a `IntervalHandle`
/// was used. This can be shared between the `FinalizationRegistry` and the  `IntervalHandle`.
class ReleasedStatus {
    released: boolean

    constructor() {
        this.released = false
    }
}

class IntervalFinalizationRegistryEntry {
    interval_name: string
    release_status: ReleasedStatus

    constructor(interval_name: string, release_status: ReleasedStatus) {
        this.interval_name = interval_name
        this.release_status = release_status
    }
}

// @ts-ignore
const handle_registry = new FinalizationRegistry(heldValue => {
    if (heldValue instanceof IntervalFinalizationRegistryEntry) {
        if (!heldValue.release_status.released) {
            console.warn(heldValue.interval_name + 'was dropped without a call to `measure`.')
        }
    }
})

class IntervalHandle {
    interval_name: string
    log_level: LogLevel
    released_status: ReleasedStatus

    constructor(interval_name: string, log_level: LogLevel) {
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
    measure() {
        end_interval(this.log_level, this.interval_name)
        this.released_status.released = true
    }

    /// Release the handle to manually call `end_interval` without emitting a warning.
    release() {
        this.released_status.released = true
    }
}

export function start_interval(log_level: LogLevel, interval_name: string): IntervalHandle {
    console.log(log_level_is_active(log_level), log_level)
    if (log_level_is_active(log_level)) {
        performance.mark(start_interval_label(log_level, interval_name))
    }
    return new IntervalHandle(interval_name, log_level)
}

export function end_interval(log_level: LogLevel, interval_name: string) {
    let start_label = start_interval_label(log_level, interval_name)
    let end_label = end_interval_label(log_level, interval_name)
    let measurement_label = measure_interval_label(log_level, interval_name)

    if (log_level_is_active(log_level)) {
        performance.mark(end_label)
        performance.measure(measurement_label, start_label, end_label)
    }
}

export function measure_interval(
    log_level: LogLevel,
    interval_name: string,
    closure: CallableFunction
) {
    start_interval(log_level, interval_name)
    const ret = closure()
    end_interval(log_level, interval_name)
    return ret
}

class Measurement {
    startTime: number
    duration: number
    logLevel: LogLevel
    name: string

    constructor(entry: PerformanceEntry) {
        const startTime = entry.startTime
        const duration = entry.duration
        const [logLevel, name] = decode_label(entry.name)

        this.startTime = startTime
        this.duration = duration
        this.logLevel = logLevel
        this.name = name
    }

    endTime(): number {
        return this.startTime + this.duration
    }

    pretty_string(): string {
        return (
            '[' +
            this.startTime.toFixed(2) +
            ', ' +
            this.endTime().toFixed(2) +
            '] (' +
            this.duration.toFixed(2) +
            ') ' +
            this.name
        )
    }

    pretty_print() {
        console.log(this.pretty_string())
    }
}

export class Report {
    measurements: Measurement[]

    constructor() {
        const entries = performance.getEntriesByType('measure')
        this.measurements = entries.map(entry => new Measurement(entry))
    }

    task_measurements() {
        return this.measurements.filter(m => m.logLevel == LogLevel.Task)
    }

    section_measurements() {
        return this.measurements.filter(m => m.logLevel == LogLevel.Section)
    }
    detail_measurements() {
        return this.measurements.filter(m => m.logLevel == LogLevel.Detail)
    }
    debug_measurements() {
        return this.measurements.filter(m => m.logLevel == LogLevel.Debug)
    }

    show() {
        console.groupCollapsed('PerformanceReport')

        console.groupCollapsed('Section')
        this.section_measurements().forEach(m => m.pretty_print())
        console.groupEnd()

        console.groupCollapsed('Task')
        this.task_measurements().forEach(m => m.pretty_print())
        console.groupEnd()

        console.groupCollapsed('Detail')
        this.detail_measurements().forEach(m => m.pretty_print())
        console.groupEnd()

        console.groupCollapsed('Debug')
        this.debug_measurements().forEach(m => m.pretty_print())
        console.groupEnd()

        console.groupEnd()
    }

    toString() {
        let output = ''
        this.section_measurements().forEach(m => (output = output.concat(m.pretty_string(), '\n')))
        this.task_measurements().forEach(m => (output = output.concat(m.pretty_string(), '\n')))
        this.detail_measurements().forEach(m => (output = output.concat(m.pretty_string(), '\n')))
        this.debug_measurements().forEach(m => (output = output.concat(m.pretty_string(), '\n')))
        return output
    }
}
