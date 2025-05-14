import type { FileFilter } from 'enso-common/src/fileFilter'

export type { FileFilter }

/** Convert to Electron type. */
export function toElectronFileFilter(filter: FileFilter): Electron.FileFilter {
  const extensions = filter.extensions === 'all' ? ['*'] : filter.extensions
  const name = filter.name ?? 'All files'
  return { extensions, name }
}
