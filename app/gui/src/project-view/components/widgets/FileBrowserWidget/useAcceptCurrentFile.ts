/** @file Encapsulates the logic for accepting the currently selected file in the File Browser. */
import { mapPath, type EnsoPath } from '@/components/widgets/FileBrowserWidget/ensoPath'
import type { ToValue } from '@/util/reactivity'
import { AssetType } from 'enso-common/src/services/Backend'
import type { Result } from 'enso-common/src/utilities/data/result'
import { ref, toValue, type Ref } from 'vue'

export type AssetExists = { exists: true; type: AssetType } | { exists: false }

export interface AcceptCurrentFileOptions {
  enteredPath: Readonly<Ref<EnsoPath | undefined>>
  fullFilePath: Readonly<Ref<string>>
  currentDirPath: Ref<EnsoPath | undefined>
  setBrowsingPath: (path: EnsoPath) => Promise<Result<void, any>>
  append: (...values: string[]) => (segments: string[]) => string[]
  setFilename: (name: string) => void
  unenteredPathSuffix: Readonly<Ref<string>>
  assetExists: (name: string) => Promise<AssetExists>
  writeMode: ToValue<boolean>
  allowOverride: ToValue<boolean>
  printEnsoPath: (path: EnsoPath) => string
  pathAcceptedCallback: (path: string) => void
}

/**
 * Encapsulates the logic for accepting the currently selected file in the File Browser.
 */
export function useAcceptCurrentFile(options: AcceptCurrentFileOptions) {
  const {
    enteredPath,
    fullFilePath,
    currentDirPath,
    setBrowsingPath,
    append,
    setFilename,
    unenteredPathSuffix,
    assetExists,
    writeMode,
    allowOverride,
    printEnsoPath,
    pathAcceptedCallback,
  } = options

  const overwriteFilename = ref<string | null>(null)
  const warningText = ref<string | null>(null)

  async function tryAcceptCurrentFile() {
    if (!enteredPath.value) {
      warningText.value = 'Unable to access files'
      return
    }
    const path = mapPath(enteredPath.value, append(...fullFilePath.value.split('/')))
    const enteringResult = await setBrowsingPath(path)
    currentDirPath.value = path
    if (!enteringResult.ok) {
      warningText.value = `${(enteringResult as any).error?.payload?.toString?.() ?? enteringResult}`
      return
    }
    setFilename(unenteredPathSuffix.value)
    const assetInfo = await assetExists(fullFilePath.value)
    if (
      assetInfo.exists &&
      assetInfo.type === AssetType.file &&
      toValue(writeMode) &&
      !toValue(allowOverride)
    ) {
      overwriteFilename.value = fullFilePath.value
    } else if (assetInfo.exists && assetInfo.type === AssetType.directory) {
      warningText.value = `'${fullFilePath.value}' is a directory, not a file`
    } else {
      acceptCurrentFile()
      return
    }
  }

  function acceptCurrentFile() {
    acceptFile(fullFilePath.value)
  }

  function acceptFile(name: string) {
    if (!enteredPath.value) return
    const currentFilePath = printEnsoPath(mapPath(enteredPath.value, append(...name.split('/'))))
    pathAcceptedCallback(currentFilePath)
  }

  return { overwriteFilename, warningText, tryAcceptCurrentFile, acceptCurrentFile, acceptFile }
}
