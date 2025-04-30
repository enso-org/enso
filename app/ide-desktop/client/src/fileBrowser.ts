import type { FileFilter } from 'enso-common/src/fileFilter'

export type { FileFilter }

/** Convert to Electron type. */
export function toElectronFileFilter(filter: FileFilter | undefined): Electron.FileFilter {
  const extensions = filter == null || filter.extensions === 'all' ? ['*'] : filter.extensions
  const name = filter?.name ?? 'All files'
  return { extensions, name }
}
