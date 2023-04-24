/** @file Logging utilities. */

import * as fsSync from 'node:fs'
import * as pathModule from 'node:path'

import * as electron from 'electron'

import * as contentConfig from 'enso-content-config'


import * as linkedDist from '../../../../../target/ensogl-pack/linked-dist'

const logger = linkedDist.logger
export function addFileLog(): string {
    const logsDirectory = electron.app.getPath('logs')
    // Create the logs directory if it doesn't exist
    if (!fsSync.existsSync(logsDirectory)) {
        fsSync.mkdirSync(logsDirectory, { recursive: true })
    }

    const filename = generateUniqueLogFilePath()
    const logFilePath = pathModule.join(logsDirectory, filename)

    contentConfig.logger.addConsumer(
        new FileConsumer(logFilePath)
    )
    return logFilePath
}

/**
 * Generate a unique log file name based on the current timestamp.
 *
 * @returns The file name log file.
 */
export function generateUniqueLogFilePath(): string {
    const timestamp = new Date().toISOString().replace(/[:.-]/g, '')
    return `IDE-${timestamp}.txt`
}

/** Log consumer that writes to a file. */
export class FileConsumer extends linkedDist.Consumer {
    private readonly logFilePath: string
    private readonly logFileHandle: number

    constructor(logPath: string) {
        super()
        this.logFilePath = logPath
        this.logFileHandle = fsSync.openSync(this.logFilePath, 'a')
        logger.log(`Log file: ${this.logFilePath}`)
    }

    override message(level: linkedDist.LogLevel, ...args: unknown[]): void {
        const timestamp = new Date().toISOString()
        const message = args
            .map(arg => (typeof arg === 'string' ? arg : JSON.stringify(arg)))
            .join(' ')
        const timestampedMessage = `[${timestamp}] [${level.toUpperCase()}] ${message}\n`

        if(this.logFileHandle) {
            try {
                fsSync.writeSync(this.logFileHandle, timestampedMessage)
            } catch (error) {
                console.error('Failed to write log:', error)
            }
        } else {
            console.error('Log file not initialized.')
        }
    }
    override startGroup(...args: unknown[]): void {
        this.message('log', '[GROUP START]', ...args)
    }
    override startGroupCollapsed(...args: unknown[]): void {
        this.message('log', '[GROUP START]', ...args)
    }
    override groupEnd(...args: unknown[]): void {
        this.message('log', '[GROUP END]', ...args)
    }
}
