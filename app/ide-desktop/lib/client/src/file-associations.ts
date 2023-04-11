/** @file
 * This module provides functionality for handling file opening events in the Enso IDE.
 *
 * It includes utilities for determining if a file can be opened, managing the file opening
 * process, and launching new instances of the IDE when necessary. The module also exports
 * constants related to file associations and project handling. */

import * as childProcess from 'node:child_process'
import * as fsSync from 'node:fs'
import * as pathModule from 'node:path'
import process from 'node:process'

import * as electron from 'electron'
import electronIsDev from 'electron-is-dev'

import * as common from 'enso-common'
import * as config from 'enso-content-config'
import * as fileAssociations from '../file-associations'
import * as project from './project-management'

const logger = config.logger

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
 *
 * @param clientArgs - A list of arguments passed to the application, stripped from the initial
 * executable name and any electron dev mode arguments.
 * @returns The path to the file to open, or `null` if no file was specified.
 */
export function argsDenoteFileOpenAttempt(clientArgs: string[]): string | null {
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
function getClientArguments(): string[] {
    if (electronIsDev) {
        // Client arguments are separated from the electron dev mode arguments by a '--' argument.
        const separator = '--'
        const separatorIndex = process.argv.indexOf(separator)
        const notFoundIndexPlaceholder = -1
        if (separatorIndex === notFoundIndexPlaceholder) {
            // If there is no separator, client gets no arguments.
            return []
        } else {
            // Drop everything before the separator.
            return process.argv.slice(separatorIndex + 1)
        }
    } else {
        // Drop the leading executable name.
        return process.argv.slice(1)
    }
}

// =========================
// === File Associations ===
// =========================

/* Check if the given path looks like a file that we can open. */
export function isFileOpenable(path: string): boolean {
    const extension = pathModule.extname(path).toLowerCase()
    return (
        extension === fileAssociations.BUNDLED_PROJECT_EXTENSION ||
        extension === fileAssociations.SOURCE_FILE_EXTENSION
    )
}

/* On macOS when Enso-associated file is opened, the application is first started and then it
 * receives the `open-file` event. However, if there is already an instance of Enso running,
 * it receives the `open-file` event (and no new instance is created for us). In this case,
 * we manually start a new instance of the application and pass the file path to it (using the
 * Windows-style command).
 */
export function onFileOpened(event: Event, path: string) {
    if (isFileOpenable(path)) {
        // If we are not ready, we can still decide to open a project rather than enter the welcome
        // screen. However, we still check for the presence of arguments, to prevent hijacking the
        // user-spawned IDE instance (OS-spawned will not have arguments set).
        if (!electron.app.isReady() && CLIENT_ARGUMENTS.length === 0) {
            event.preventDefault()
            logger.log(`Opening file '${path}'.`)
            // eslint-disable-next-line no-restricted-syntax
            return handleOpenFile(path)
        } else {
            // We need to start another copy of the application, as the first one is already running.
            logger.log(
                `The application is already initialized. Starting a new instance to open file '${path}'.`
            )
            const args = [path]
            const child = childProcess.spawn(process.execPath, args, {
                detached: true,
                stdio: 'ignore',
            })
            // Prevent parent (this) process from waiting for the child to exit.
            child.unref()
        }
    }
}

/** Handle the case where IDE is invoked with a file to open.
 *
 * Imports project if necessary. Returns the ID of the project to open. In case of an error, displays an error message and rethrows the error.
 *
 * @throws An `Error`, if the project from the file cannot be opened or imported. */
export function handleOpenFile(openedFile: string): string {
    try {
        return project.importProjectFromPath(openedFile)
    } catch (e: unknown) {
        // Since the user has explicitly asked us to open a file, in case of an error, we should
        // display a message box with the error details.
        let message = `Cannot open file '${openedFile}'.`
        message += `\n\nReason:\n${e?.toString() ?? 'Unknown error'}`
        if (e instanceof Error && typeof e.stack !== 'undefined') {
            message += `\n\nDetails:\n${e.stack}`
        }
        logger.error(e)
        electron.dialog.showErrorBox(common.PRODUCT_NAME, message)
        throw e
    }
}
