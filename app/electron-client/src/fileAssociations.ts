/**
 * @file
 * This module provides functionality for handling file opening events in the Enso IDE.
 *
 * It includes utilities for determining if a file can be opened, managing the file opening
 * process, and launching new instances of the IDE when necessary. The module also exports
 * constants related to file associations and project handling.
 */
import type { Electron } from '@/electron'
import type { Event } from 'electron'
import * as common from 'enso-common/src/constants'
import * as fsSync from 'node:fs'
import * as pathModule from 'node:path'
import * as project from 'project-manager-shim'
import * as fileAssociations from '../fileAssociations'
export * from '../fileAssociations'

/** Returned by {@link String.indexOf} when the substring was not found. */
const NOT_FOUND = -1

/**
 * Check if the given list of application startup arguments denotes an attempt to open a file.
 *
 * For example, this happens when the user double-clicks on a file in the file explorer and the
 * application is launched with the file path as an argument.
 * @param clientArgs - A list of arguments passed to the application, stripped from the initial
 * executable name and any electron dev mode arguments.
 * @returns The path to the file to open, or `null` if no file was specified.
 */
export function getFileToOpen(clientArgs: readonly string[]): string | null {
  const arg = clientArgs[0]
  let result: string | null = null
  // If the application is invoked with exactly one argument and this argument is a file, we
  // assume that we have been launched with a file to open. In this case, we must translate this
  // path to the actual argument that'd open the project containing this file.
  if (clientArgs.length === 1 && typeof arg !== 'undefined') {
    try {
      fsSync.accessSync(arg, fsSync.constants.R_OK)
      result = arg
    } catch {
      // Do nothing.
    }
  }
  return result
}

/** Parse client arguments. */
export function parseClientArguments(
  args: readonly string[],
  electronIsDev: boolean,
): readonly string[] {
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
        .filter((option) => !/^--no-sandbox$|^--inspect|^--remote-debugging-port/.test(option))
    )
  }
}

/** Check if the given path looks like a file that we can open. */
export function isFileOpenable(path: string): boolean {
  const extension = pathModule.extname(path).toLowerCase()
  return (
    extension === fileAssociations.BUNDLED_PROJECT_SUFFIX ||
    extension === fileAssociations.SOURCE_FILE_SUFFIX
  )
}

/** Callback called when a file is opened via the `open-file` event. */
export function onFileOpened(event: Event, path: string): string | null {
  console.log(`Received 'open-file' event for path '${path}'.`)
  if (isFileOpenable(path)) {
    console.log(`The file '${path}' is openable.`)
    event.preventDefault()
    console.log(`Opening file '${path}'.`)
    return path
  } else {
    console.log(`The file '${path}' is not openable, ignoring the 'open-file' event.`)
    return null
  }
}

/**
 * Set up the `open-file` event handler that might import a project and invoke the given callback,
 * if this IDE instance should load the project. See {@link onFileOpened} for more details.
 * @param setProjectToOpen - A function that will be called with the path of the project to open.
 */
export function setOpenFileEventHandler(
  setProjectToOpen: (path: string) => void,
  electron: Electron,
) {
  electron.app.on('open-file', (_event, path) => {
    console.log(`Opening file '${path}'.`)
    setProjectToOpen(path)
  })

  electron.app.on('second-instance', (event, _argv, _workingDir, additionalData) => {
    // Check if additional data is an object that contains the URL.
    console.log(`Checking path`, additionalData)
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
      console.log(`Got path '${path.toString()}' from second instance.`)
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
export function handleOpenFile(openedFile: string, electron: Electron): project.ProjectInfo {
  try {
    const title = openedFile
      .split(pathModule.sep)
      .pop()
      ?.replace(`.${fileAssociations.BUNDLED_PROJECT_EXTENSION}`, '')
    return project.importProjectFromPath(openedFile, null, title)
  } catch (error) {
    // Since the user has explicitly asked us to open a file, in case of an error, we should
    // display a message box with the error details.
    let message = `Cannot open file '${openedFile}'.`
    message += `\n\nReason:\n${error?.toString() ?? 'Unknown error'}`
    if (error instanceof Error && typeof error.stack !== 'undefined') {
      message += `\n\nDetails:\n${error.stack}`
    }
    console.error(error)
    electron.dialog.showErrorBox(common.PRODUCT_NAME, message)
    throw error
  }
}
