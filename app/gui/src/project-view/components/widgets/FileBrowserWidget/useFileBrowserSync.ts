/** @file Orchestrates reactive synchronization for the File Browser widget. */
import { mapPath, type EnsoPath } from '@/components/widgets/FileBrowserWidget/ensoPath'
import type { ToValue } from '@/util/reactivity'
import type { Result } from 'enso-common/src/utilities/data/result'
import { toValue, watchEffect, type Ref } from 'vue'

export interface FileBrowserSyncOptions {
  /** Whether the widget is in write mode (affects filename syncing). */
  writeMode: ToValue<boolean>
  /** The externally chosen path (enso://...) to sync into the browser. */
  choosenPath: ToValue<string>
  /** Parses enso:// strings into EnsoPath results (already unit-tested separately). */
  parseEnsoPath: (path: string) => Result<EnsoPath, string>

  /** The currently selected directory path in the UI. */
  currentDirPath: Readonly<Ref<EnsoPath | undefined>>
  /** The currently chosen filename in the UI (if any). */
  chosenFilename: Readonly<Ref<string | null>>

  /** Called to update the browser's path from an external input. */
  setPath: (path: Result<EnsoPath, string>) => void
  /** Called to update the browsing path to reflect the current UI state. */
  setBrowsingPath: (path: EnsoPath) => void
  /** Function that appends a value to path segments. */
  append: (name: string) => (segments: string[]) => string[]

  /** Called to update the filename input. */
  setFilename: (name: string) => void
  /** Suffix of the path that couldn't be entered (e.g., a filename). */
  unenteredPathSuffix: Readonly<Ref<string>>
}

/**
 * Installs watchers that keep the browser state synchronized with incoming props and
 * user interactions. Extracted from `FileBrowserWidget.vue` for independent testing.
 */
export function useFileBrowserSync(options: FileBrowserSyncOptions) {
  const {
    writeMode,
    choosenPath,
    parseEnsoPath,
    currentDirPath,
    chosenFilename,
    setPath,
    setBrowsingPath,
    append,
    setFilename,
    unenteredPathSuffix,
  } = options

  // Sync opened directory with the passed property.
  watchEffect(() => setPath(parseEnsoPath(toValue(choosenPath))))

  // Sync the browsing path with the current directory (usually when navigating).
  watchEffect(() => {
    const dirPath = currentDirPath.value
    if (dirPath) {
      const name = chosenFilename.value
      const fullPath = name ? mapPath(dirPath, append(name)) : dirPath
      setBrowsingPath(fullPath)
    }
  })

  // Sync the filename with entered path (usually when opening the file browser).
  watchEffect(() => {
    if (toValue(writeMode) && unenteredPathSuffix.value) setFilename(unenteredPathSuffix.value)
  })

  // Set the filename with the chosen file.
  watchEffect(() => {
    if (chosenFilename.value) setFilename(chosenFilename.value)
  })
}
