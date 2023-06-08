/** @file Logging utilities.
 *
 * This module includes a special {@link addFileLog function} that adds a new log consumer that
 * writes to a file.
 *
 * This is the primary entry point, though its building blocks are also exported,
 * like {@link FileConsumer}. */

import * as fsSync from 'node:fs'
import * as pathModule from 'node:path'

import * as contentConfig from 'enso-content-config'
import * as paths from 'paths'

import * as linkedDist from '../../../../../target/ensogl-pack/linked-dist'

// ================
// === Log File ===
// ================

/** Adds a new log consumer that writes to a file.
 *
 * The path of the log file is {@link generateUniqueLogFileName automatically generated}.
 *
 * The log file is created in the {@link paths.LOGS_DIRECTORY logs directory}
 *
 * @returns The full path of the log file. */
export function addFileLog(): string {
    const dirname = paths.LOGS_DIRECTORY
    const filename = generateUniqueLogFileName()
    const logFilePath = pathModule.join(dirname, filename)
    const consumer = new FileConsumer(logFilePath)
    contentConfig.logger.addConsumer(consumer)
    return logFilePath
}

/** Generate a unique log file name based on the current timestamp.
 *
 * @returns The file name log file. */
export function generateUniqueLogFileName(): string {
    // Replace ':' with '-' because ':' is not allowed in file names.
    const timestamp = new Date().toISOString().replace(/:/g, '-')
    const version = contentConfig.VERSION.ide.raw
    return `${timestamp}-ide-${version}.log`
}

// ================
// === Consumer ===
// ================

/** Log consumer that writes to a file. */
export class FileConsumer extends linkedDist.Consumer {
    private readonly logFilePath: string
    private readonly logFileHandle: number

    /** Create a log consumer that writes to a file.
     *
     * @param logPath - The path of the log file. Must be writeable. */
    constructor(logPath: string) {
        super()
        // Create the directory if it doesn't exist, otherwise fsSync.openSync will fail.
        const logsDirectory = pathModule.dirname(logPath)
        fsSync.mkdirSync(logsDirectory, { recursive: true })
        this.logFilePath = logPath
        this.logFileHandle = fsSync.openSync(this.logFilePath, 'a')
    }

    /** Append a message to the log. */
    override message(level: linkedDist.LogLevel, ...args: unknown[]): void {
        const timestamp = new Date().toISOString()
        const message = args
            .map(arg => (typeof arg === 'string' ? arg : JSON.stringify(arg)))
            .join(' ')
        const timestampedMessage = `[${timestamp}] [${level.toUpperCase()}] ${message}\n`

        if (this.logFileHandle) {
            try {
                fsSync.writeSync(this.logFileHandle, timestampedMessage)
            } catch (error) {
                console.error('Failed to write log:', error)
            }
        } else {
            // This should never happen, as the log file handle is initialized in the constructor.
            console.error('Log file not initialized.')
        }
    }

    /** Start a log group. */
    override startGroup(...args: unknown[]): void {
        this.message('log', '[GROUP START]', ...args)
    }

    /** Start a collapsed log group - for `FileConsumer`, this does the same thing
     * as `startGroup`. */
    override startGroupCollapsed(...args: unknown[]): void {
        // We don't have a way to collapse groups in the file logger, so we just use the same
        // function as startGroup.
        this.message('log', '[GROUP START]', ...args)
    }

    /** End a log group. */
    override groupEnd(...args: unknown[]): void {
        this.message('log', '[GROUP END]', ...args)
    }
}
