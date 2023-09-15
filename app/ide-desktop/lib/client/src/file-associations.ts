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
import * as contentConfig from 'enso-content-config'

import * as clientConfig from './config'
import * as fileAssociations from '../file-associations'
import * as project from './project-management'

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

/** Check if the given list of application startup arguments denotes an attempt to open a file.
 *
 * For example, this happens when the user double-clicks on a file in the file explorer and the
 * application is launched with the file path as an argument.
 *
 * @param clientArgs - A list of arguments passed to the application, stripped from the initial
 * executable name and any electron dev mode arguments.
 * @returns The path to the file to open, or `null` if no file was specified. */
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
        if (separatorIndex === NOT_FOUND) {
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

/** Check if the given path looks like a file that we can open. */
export function isFileOpenable(path: string): boolean {
    const extension = pathModule.extname(path).toLowerCase()
    return (
        extension === fileAssociations.BUNDLED_PROJECT_SUFFIX ||
        extension === fileAssociations.SOURCE_FILE_SUFFIX
    )
}

/** On macOS when an Enso-associated file is opened, the application is first started and then it
 * receives the `open-file` event. However, if there is already an instance of Enso running,
 * it receives the `open-file` event (and no new instance is created for us). In this case,
 * we manually start a new instance of the application and pass the file path to it (using the
 * Windows-style command). */
export function onFileOpened(event: electron.Event, path: string): string | null {
    logger.log(`Received 'open-file' event for path '${path}'.`)
    if (isFileOpenable(path)) {
        logger.log(`The file '${path}' is openable.`)
        // If we are not ready, we can still decide to open a project rather than enter the welcome
        // screen. However, we still check for the presence of arguments, to prevent hijacking the
        // user-spawned IDE instance (OS-spawned will not have arguments set).
        if (!electron.app.isReady() && CLIENT_ARGUMENTS.length === 0) {
            event.preventDefault()
            logger.log(`Opening file '${path}'.`)
            return handleOpenFile(path)
        } else {
            // Another copy of the application needs to be started, as the first one is
            // already running.
            logger.log(
                "The application is already initialized. Starting a new instance to open file '" +
                    path +
                    "'."
            )
            const args = [path]
            const child = childProcess.spawn(process.execPath, args, {
                detached: true,
                stdio: 'ignore',
            })
            // Prevent parent (this) process from waiting for the child to exit.
            child.unref()
            return null
        }
    } else {
        logger.log(`The file '${path}' is not openable, ignoring the 'open-file' event.`)
        return null
    }
}

/** Set up the `open-file` event handler that might import a project and invoke the given callback,
 * if this IDE instance should load the project. See {@link onFileOpened} for more details.
 *
 * @param setProjectToOpen - A function that will be called with the ID of the project to open. */
export function setOpenFileEventHandler(setProjectToOpen: (id: string) => void) {
    electron.app.on('open-file', (event, path) => {
        const projectId = onFileOpened(event, path)
        if (typeof projectId === 'string') {
            setProjectToOpen(projectId)
        }
    })
}

/** Handle the case where IDE is invoked with a file to open.
 *
 * Imports project if necessary. Returns the ID of the project to open. In case of an error,
 * the error message is displayed and the error is re-thrown.
 *
 * @param openedFile - The path to the file to open.
 * @returns The ID of the project to open.
 * @throws {Error} if the project from the file cannot be opened or imported. */
export function handleOpenFile(openedFile: string): string {
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

/** Handle the file to open, if any. See {@link handleOpenFile} for details.
 *
 * If no file to open is provided, does nothing.
 *
 * Handles all errors internally.
 * @param openedFile - The file to open (null if none).
 * @param args - The parsed application arguments. */
export function handleFileArguments(openedFile: string | null, args: clientConfig.Args): void {
    if (openedFile != null) {
        try {
            // This makes the IDE open the relevant project. Also, this prevents us from using this
            // method after IDE has been fully set up, as the initializing code would have already
            // read the value of this argument.
            args.groups.startup.options.project.value = handleOpenFile(openedFile)
        } catch (e) {
            // If we failed to open the file, we should enter the usual welcome screen.
            // The `handleOpenFile` function will have already displayed an error message.
        }
    }
}
