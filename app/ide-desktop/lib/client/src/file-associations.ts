import pathModule from 'node:path'
import * as paths from './paths'
import * as electron from 'electron'
import child_process from 'node:child_process'
import process from 'node:process'
import { BUNDLED_PROJECT_EXTENSION, SOURCE_FILE_EXTENSION } from '../file-associations'
import * as project from './project-management'
import {logger} from "enso-content-config";
import {PRODUCT_NAME} from "enso-common";

export * from '../file-associations'

/* Check if the given path looks like a file that we can open. */
export function isFileOpenable(path: string): boolean {
    const extension = pathModule.extname(path).toLowerCase()
    return extension === BUNDLED_PROJECT_EXTENSION || extension === SOURCE_FILE_EXTENSION
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
        if (!electron.app.isReady() && paths.clientArguments.length === 0) {
            event.preventDefault()
            console.log(`Opening file '${path}'.`)
            return handleOpenFile(path)
        } else {
            // We need to start another copy of the application, as the first one is already running.
            console.log(`We are already initialized, opening '${path}' in a new instance.`)
            const args = [path]
            const child = child_process.spawn(process.execPath, args, {
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
 * Imports project if necessary. Returns the ID of the project to open. In case of an error, displays an error message and rethrows the error. */
export function handleOpenFile(openedFile: string): string {
    try {
        return project.importProjectFromPath(openedFile)
    } catch (e: unknown) {
        // Since the user has explicitly asked us to open a file, in case of an error, we should
        // display a message box with the error details.
        let message = `Cannot open file: ${e}.`
        if (e instanceof Error) {
            message += `\n\nDetails:\n${e.stack}`
        }
        logger.error(e)
        electron.dialog.showErrorBox(PRODUCT_NAME, message)
        throw e
    }
}
