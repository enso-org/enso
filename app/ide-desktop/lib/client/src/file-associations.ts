/** @file
 * This module provides functionality for handling file opening events in the Enso IDE.
 *
 * It includes utilities for determining if a file can be opened, managing the file opening
 * process, and launching new instances of the IDE when necessary. The module also exports
 * constants related to file associations and project handling. */

import * as childProcess from 'node:child_process'
import * as pathModule from 'node:path'
import process from 'node:process'

import * as electron from 'electron'

import * as common from 'enso-common'
import * as config from 'enso-content-config'
import * as fileAssociations from '../file-associations'
import * as paths from './paths'
import * as project from './project-management'

const logger = config.logger

// =================
// === Reexports ===
// =================

export const BUNDLED_PROJECT_EXTENSION = fileAssociations.BUNDLED_PROJECT_EXTENSION
export const SOURCE_FILE_EXTENSION = fileAssociations.SOURCE_FILE_EXTENSION
export const BUNDLED_PROJECT_SUFFIX = fileAssociations.BUNDLED_PROJECT_SUFFIX
export const SOURCE_FILE_SUFFIX = fileAssociations.SOURCE_FILE_SUFFIX

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
        if (!electron.app.isReady() && paths.CLIENT_ARGUMENTS.length === 0) {
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
