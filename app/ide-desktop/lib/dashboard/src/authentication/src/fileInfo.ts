/** @file Utility functions for extracting and manipulating file information. */
import FileIcon from 'enso-assets/file.svg'

// ================================
// === Extract file information ===
// ================================

/** Extract the file extension from a file name. */
export function fileExtension(fileName: string) {
    return fileName.match(/\.([^.]+?)$/)?.[1] ?? ''
}

/** Returns the appropriate icon for a specific file extension. */
export function fileIcon() {
    return FileIcon
}
