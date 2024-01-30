/** @file Utility functions for extracting and manipulating file information. */

// ================================
// === Extract file information ===
// ================================

/** Return just the file name, without the path and without the extension. */
export function baseName(fileName: string) {
  return fileName.match(/(?:\/|^)([^./]+)(?:\.[^/]*)?$/)?.[1] ?? fileName
}

/** Extract the file extension from a file name. */
export function fileExtension(fileName: string) {
  return fileName.match(/\.([^.]+?)$/)?.[1] ?? ''
}

/** Return both the name and extension of the file name (if any).
 * Otherwise, returns the entire name as the basename. */
export function basenameAndExtension(name: string) {
  const [, basename, extension] = name.match(/^([^.]*)[.](.+)$/) ?? []
  return { basename: basename ?? name, extension: extension ?? '' }
}
