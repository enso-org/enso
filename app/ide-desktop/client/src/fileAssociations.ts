/**
 * @file
 * This module provides functionality for handling file opening events in the Enso IDE.
 *
 * It includes utilities for determining if a file can be opened, managing the file opening
 * process, and launching new instances of the IDE when necessary. The module also exports
 * constants related to file associations and project handling.
 */
import * as fsSync from 'node:fs'
import * as pathModule from 'node:path'
import process from 'node:process'

import * as electron from 'electron'
import electronIsDev from 'electron-is-dev'

import * as common from 'enso-common'

import * as contentConfig from '@/contentConfig'
import * as project from '@/projectManagement'
import * as fileAssociations from '../fileAssociations'

const logger = contentConfig.logger

// =================
// === Constants ===
// =================

/** Returned by {@link String.indexOf} when the substring was not found. */
const NOT_FOUND = -1

// =================
// === Reexports ===
// =================

export const BUNDLED_PROJECT_EXTENSION = fileAssociations.BUNDLED_PROJECT_EXTENSION
export const SOURCE_FILE_EXTENSION = fileAssociations.SOURCE_FILE_EXTENSION
export const BUNDLED_PROJECT_SUFFIX = fileAssociations.BUNDLED_PROJECT_SUFFIX
export const SOURCE_FILE_SUFFIX = fileAssociations.SOURCE_FILE_SUFFIX

// ==========================
// === Arguments Handling ===
// ==========================

/**
 * Check if the given list of application startup arguments denotes an attempt to open a file.
 *
 * For example, this happens when the user double-clicks on a file in the file explorer and the
 * application is launched with the file path as an argument.
 * @param clientArgs - A list of arguments passed to the application, stripped from the initial
 * executable name and any electron dev mode arguments.
 * @returns The path to the file to open, or `null` if no file was specified.
 */
export function argsDenoteFileOpenAttempt(clientArgs: readonly string[]): string | null {
  const arg = clientArgs[0]
  let result: string | null = null
  // If the application is invoked with exactly one argument and this argument is a file, we
  // assume that we have been launched with a file to open. In this case, we must translate this
  // path to the actual argument that'd open the project containing this file.
  if (clientArgs.length === 1 && typeof arg !== 'undefined') {
    try {
      fsSync.accessSync(arg, fsSync.constants.R_OK)
      result = arg
    } catch (e) {
      logger.log(`The single argument '${arg}' does not denote a readable file: ${String(e)}`)
    }
  }
  return result
}

/** Get the arguments, excluding the initial program name and any electron dev mode arguments. */
export const CLIENT_ARGUMENTS = getClientArguments()

/** Decide what are client arguments, @see {@link CLIENT_ARGUMENTS}. */
function getClientArguments(args = process.argv): readonly string[] {
  if (electronIsDev) {
    // Client arguments are separated from the electron dev mode arguments by a '--' argument.
    const separator = '--'
    const separatorIndex = args.indexOf(separator)
    if (separatorIndex === NOT_FOUND) {
      // If there is no separator, client gets no arguments.
      return []
    } else {
      // Drop everything before the separator.
      return args.slice(separatorIndex + 1)
    }
  } else {
    // Drop the leading executable name and known electron options.
    return (
      args
        .slice(1)
        // Omitting $ in --inspect and --remote-debugging-port is intentional.
        .filter(option => !/^--no-sandbox$|^--inspect|^--remote-debugging-port/.test(option))
    )
  }
}

// =========================
// === File Associations ===
// =========================

/** Check if the given path looks like a file that we can open. */
export function isFileOpenable(path: string): boolean {
  const extension = pathModule.extname(path).toLowerCase()
  return (
    extension === fileAssociations.BUNDLED_PROJECT_SUFFIX ||
    extension === fileAssociations.SOURCE_FILE_SUFFIX
  )
}

/** Callback called when a file is opened via the `open-file` event. */
export function onFileOpened(event: electron.Event, path: string): string | null {
  logger.log(`Received 'open-file' event for path '${path}'.`)
  if (isFileOpenable(path)) {
    logger.log(`The file '${path}' is openable.`)
    event.preventDefault()
    logger.log(`Opening file '${path}'.`)
    return path
  } else {
    logger.log(`The file '${path}' is not openable, ignoring the 'open-file' event.`)
    return null
  }
}

/**
 * Set up the `open-file` event handler that might import a project and invoke the given callback,
 * if this IDE instance should load the project. See {@link onFileOpened} for more details.
 * @param setProjectToOpen - A function that will be called with the path of the project to open.
 */
export function setOpenFileEventHandler(setProjectToOpen: (path: string) => void) {
  electron.app.on('open-file', (_event, path) => {
    logger.log(`Opening file '${path}'.`)
    setProjectToOpen(path)
  })

  electron.app.on('second-instance', (event, _argv, _workingDir, additionalData) => {
    // Check if additional data is an object that contains the URL.
    logger.log(`Checking path`, additionalData)
    const path =
      (
        additionalData != null &&
        typeof additionalData === 'object' &&
        'fileToOpen' in additionalData &&
        typeof additionalData.fileToOpen === 'string'
      ) ?
        additionalData.fileToOpen
      : null
    if (path != null) {
      logger.log(`Got path '${path.toString()}' from second instance.`)
      event.preventDefault()
      const file = onFileOpened(event, path)
      if (file != null) {
        setProjectToOpen(file)
      }
    }
  })
}

/**
 * Handle the case where IDE is invoked with a file to open.
 *
 * Imports project if necessary. Returns the ID of the project to open. In case of an error,
 * the error message is displayed and the error is re-thrown.
 * @param openedFile - The path to the file to open.
 * @returns The ID of the project to open.
 * @throws {Error} if the project from the file cannot be opened or imported.
 */
export function handleOpenFile(openedFile: string): project.ProjectInfo {
  try {
    return project.importProjectFromPath(openedFile)
  } catch (error) {
    // Since the user has explicitly asked us to open a file, in case of an error, we should
    // display a message box with the error details.
    let message = `Cannot open file '${openedFile}'.`
    message += `\n\nReason:\n${error?.toString() ?? 'Unknown error'}`
    if (error instanceof Error && typeof error.stack !== 'undefined') {
      message += `\n\nDetails:\n${error.stack}`
    }
    logger.error(error)
    electron.dialog.showErrorBox(common.PRODUCT_NAME, message)
    throw error
  }
}
