/**
 * @file Logging utilities.
 *
 * This module includes a special {@link addFileLog function} that adds a new log consumer that
 * writes to a file.
 *
 * This is the primary entry point, though its building blocks are also exported,
 * like {@link Logger}.
 */
import * as contentConfig from '@/contentConfig'
import type { Electron } from '@/electron'
import * as paths from '@/paths'
import * as fsSync from 'node:fs'
import * as pathModule from 'node:path'
import * as util from 'node:util'

const consoleLog = console.log
const consoleError = console.error

type LogLevel = 'log' | 'info' | 'error' | 'warn' | 'debug'

/**
 * Setup logging, overriding standard console methods with our own.
 *
 * Only the following methods are overridden:
 * - `console.log`
 * - `console.info`
 * - `console.error`
 * - `console.warn`
 * - `console.debug`
 *
 * The path of the log file is {@link generateUniqueLogFileName automatically generated}.
 *
 * The log file is created in the {@link paths.logsPath logs directory}
 */
export function setupLogger(electron: Electron | undefined): void {
  const dirname = paths.logsPath(electron)
  const filename = generateUniqueLogFileName()
  const logFilePath = pathModule.join(dirname, filename)
  const consumer = new Logger(logFilePath)
  function handleException(f: () => void) {
    try {
      f()
    } catch (error) {
      consoleError('Error in logger: ', error)
    }
  }
  console.log = (...args) => {
    handleException(() => consumer.message('log', ...args))
  }
  console.info = (...args) => {
    handleException(() => consumer.message('info', ...args))
  }
  console.error = (...args) => {
    handleException(() => consumer.message('error', ...args))
  }
  console.warn = (...args) => {
    handleException(() => consumer.message('warn', ...args))
  }
  // eslint-disable-next-line no-restricted-properties
  console.debug = (...args) => {
    handleException(() => consumer.message('debug', ...args))
  }
}

/**
 * Generate a unique log file name based on the current timestamp.
 * @returns The file name log file.
 */
export function generateUniqueLogFileName(): string {
  // Replace ':' with '-' because ':' is not allowed in file names.
  const timestamp = new Date().toISOString().replace(/:/g, '-')
  const version = contentConfig.VERSION.ide.raw
  return `${timestamp}-ide-${version}.log`
}

/** Logger that writes both to file and stdout. */
export class Logger {
  private readonly logFilePath: string
  private readonly logFileHandle: number

  /**
   * @param logPath - The path of the log file. Must be writeable.
   */
  constructor(logPath: string) {
    // Create the directory if it doesn't exist, otherwise fsSync.openSync will fail.
    const logsDirectory = pathModule.dirname(logPath)
    fsSync.mkdirSync(logsDirectory, { recursive: true })
    this.logFilePath = logPath
    this.logFileHandle = fsSync.openSync(this.logFilePath, 'a')
  }

  /** Write a message to the stdout and the log file. */
  message(level: LogLevel, ...args: unknown[]): void {
    const timestamp = new Date().toISOString()
    const message = util.format(...args)
    const timestampedMessage = `[${level.toUpperCase()}] [${timestamp}] ${message}`
    consoleLog(timestampedMessage)
    fsSync.writeSync(this.logFileHandle, timestampedMessage + '\n')
  }
}
