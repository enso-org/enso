/** @file Utility functions for extracting and manipulating file information. */

// ================================
// === Extract file information ===
// ================================

/** Return just the file name, including the extension. */
export function getFileName(filePath: string) {
  return filePath.match(/(?:[/\\]|^)([^/\\]+)[/\\]?$/)?.[1] ?? filePath
}

/** Return the entire path, without the file name. */
export function getFolderPath(filePath: string) {
  return filePath.match(/^.+[/\\]/)?.[0] ?? filePath
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
