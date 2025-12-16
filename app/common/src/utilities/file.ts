import { newtypeConstructor, type Newtype } from './data/newtype.js'
import { isOnWindows } from './detect.js'

/** A filesystem path. */
export type Path = Newtype<string, 'Path'>
export const Path = newtypeConstructor<Path>()

/** @file Functions related to files. */
export type FileExtension = `.${string}`
export type MimeType = `${string}/${string}`

export interface InputFilesOptions {
  readonly accept?: (FileExtension | MimeType)[] | []
  readonly multiple?: boolean
}

/** Open a file-selection dialog and read the file selected by the user. */
export function readUserSelectedFile(options: InputFilesOptions = {}) {
  return new Promise<FileList>((resolve, reject) => {
    const input = document.createElement('input')
    input.type = 'file'
    input.multiple = options.multiple ?? false
    input.style.display = 'none'
    if (options.accept) input.accept = options.accept.join(',')
    document.body.appendChild(input)
    input.addEventListener('input', () => {
      resolve(input.files!)
    })
    input.addEventListener('cancel', () => {
      reject(new Error('File selection was cancelled.'))
    })
    input.click()
    input.remove()
  })
}

/** Return just the file name, including the extension. */
export function getFileName(filePath: string) {
  return filePath.match(/(?:[/\\]|^)([^/\\]+)[/\\]?$/)?.[1] ?? filePath
}

/** Whether a path represents a folder. */
export function isFolderPath(path: string) {
  return /[/\\]$/.test(path)
}

/** Return the entire path, without the file name. */
export function getFolderPath(filePath: string) {
  return filePath.match(/^.+[/\\](?=.)/)?.[0] ?? ''
}

/** Return the root folder in the path, or the entire path if there are no folders. */
export function getRootEntryInPath(filePath: string) {
  return filePath.match(/^.+?[/\\]/)?.[0] ?? filePath
}

/** Return just the file name, without the path and without the extension. */
export function baseName(fileNameOrPath: string) {
  return fileNameOrPath.match(/(?:[\\/]|^)([^./\\]+)(?:[.][^/\\]*)?$/)?.[1] ?? fileNameOrPath
}

/** Normalize a path to use `/` instead of `\`. */
export function normalizePath(path: string) {
  return path.replace(/\\/g, '/')
}

/** Extract the file extension from a file name. */
export function fileExtension(fileNameOrPath: string) {
  return fileNameOrPath.match(/[.]([^.]+?)$/)?.[1] ?? ''
}

/**
 * Return both the name and extension of the file name (if any).
 * Otherwise, returns the entire name as the basename.
 */
export function basenameAndExtension(name: string) {
  const [, basename, extension] = name.match(/^([^.]*)[.](.+)$/) ?? []
  return { basename: basename ?? name, extension: extension ?? '' }
}

/** Construct a {@link Path} from an existing {@link Path} of the parent directory. */
export function joinPath(directoryPath: Path, fileName: string) {
  return Path(`${directoryPath}/${fileName}`)
}

/** Return the path, with backslashes (on Windows only) normalized to forward slashes. */
export function normalizeSlashes(path: string): Path {
  if (isOnWindows()) {
    return Path(path.replace(/\\/g, '/'))
  } else {
    return Path(path)
  }
}

/** Split a {@link Path} inito the path of its parent directory, and its file name. */
export function getDirectoryAndName(path: Path) {
  const [, directoryPath = '', fileName = ''] = path.match(/^(.+)[/]([^/]+)$/) ?? []
  return { directoryPath: Path(directoryPath), fileName }
}
