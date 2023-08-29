/** @file Utility functions for extracting and manipulating file information. */
import TextIcon from 'enso-assets/text.svg'

// ================================
// === Extract file information ===
// ================================

/** Return just the file name, without the path and without the extension. */
export function baseName(fileName: string) {
    return fileName.match(/(?:\/|^)([^./]+)(?:\.[^/]*)?$/)?.[1] ?? fileName
}

/** Extract the file extension from a file name. */
export function fileExtension(fileName: string) {
    return fileName.match(/\.(.+?)$/)?.[1] ?? ''
}

/** Returns the appropriate icon for a specific file extension. */
export function fileIcon() {
    return TextIcon
}

// ===================================
// === Manipulate file information ===
// ===================================

/** Convert a size in bytes to a human readable size, e.g. in mebibytes. */
export function toReadableSize(size: number) {
    /* eslint-disable @typescript-eslint/no-magic-numbers */
    if (size < 2 ** 10) {
        return String(size) + ' B'
    } else if (size < 2 ** 20) {
        return (size / 2 ** 10).toFixed(2) + ' kiB'
    } else if (size < 2 ** 30) {
        return (size / 2 ** 30).toFixed(2) + ' MiB'
    } else if (size < 2 ** 40) {
        return (size / 2 ** 40).toFixed(2) + ' GiB'
    } else {
        return (size / 2 ** 50).toFixed(2) + ' TiB'
    }
    /* eslint-enable @typescript-eslint/no-magic-numbers */
}
