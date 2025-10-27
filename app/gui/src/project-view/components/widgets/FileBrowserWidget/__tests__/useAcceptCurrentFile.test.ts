import { AssetType, DirectoryId } from 'enso-common/src/services/Backend'
import { Err, Ok, type Result } from 'enso-common/src/utilities/data/result'
import { uuidv4 } from 'lib0/random'
import { describe, expect, test, vi } from 'vitest'
import { ref, type Ref } from 'vue'
import type { EnsoPath } from '../ensoPath'
import { useAcceptCurrentFile, type AssetExists } from '../useAcceptCurrentFile'

const rootId = DirectoryId(`directory-${uuidv4()}`)
function path(segments: string[] = []): EnsoPath {
  return { root: rootId, segments }
}

describe('useAcceptCurrentFile', () => {
  function setupAccept(options?: {
    enteredPath?: Ref<EnsoPath | undefined>
    fullFilePath?: Ref<string>
    currentDirPath?: Ref<EnsoPath | undefined>
    setBrowsingPath?: () => Promise<Result<void, any>>
    assetExists?: () => Promise<AssetExists>
    writeMode?: boolean
    allowOverride?: boolean
    printEnsoPath?: (p: EnsoPath) => string
    pathAcceptedCallback?: (p: string) => void
  }) {
    const enteredPath = options?.enteredPath ?? ref(path([]))
    const fullFilePath = options?.fullFilePath ?? ref('file.txt')
    const currentDirPath = options?.currentDirPath ?? ref(undefined)
    const setBrowsing = options?.setBrowsingPath ?? (async () => Ok())
    const exists = options?.assetExists ?? (async () => ({ exists: false }))
    const writeMode = options?.writeMode ?? true
    const allowOverride = options?.allowOverride ?? false
    const print = options?.printEnsoPath ?? (() => '')
    const emit = options?.pathAcceptedCallback ?? (() => {})

    const api = useAcceptCurrentFile({
      enteredPath,
      fullFilePath,
      currentDirPath,
      setBrowsingPath: setBrowsing,
      append: (n: string) => (s: string[]) => [...s, n],
      setFilename: () => {},
      unenteredPathSuffix: ref(''),
      assetExists: exists,
      writeMode: () => writeMode,
      allowOverride: () => allowOverride,
      printEnsoPath: print,
      pathAcceptedCallback: emit,
    })

    return { enteredPath, fullFilePath, currentDirPath, api }
  }

  test('warns if enteredPath is missing', async () => {
    const { api } = setupAccept({
      enteredPath: ref(undefined),
      printEnsoPath: () => 'enso://file.txt',
    })
    await api.tryAcceptCurrentFile()
    expect(api.warningText.value).toBe('Unable to access files')
  })

  test('accepts when asset does not exist', async () => {
    const onPathAccepted = vi.fn()
    const print = vi.fn(() => 'enso://dir/file.txt')
    const { api } = setupAccept({
      enteredPath: ref(path(['dir'])),
      fullFilePath: ref('file.txt'),
      pathAcceptedCallback: onPathAccepted,
      printEnsoPath: print,
    })
    await api.tryAcceptCurrentFile()
    expect(print).toHaveBeenCalledWith(path(['dir', 'file.txt']))
    expect(onPathAccepted).toHaveBeenCalledWith('enso://dir/file.txt')
  })

  test('sets overwrite dialog when file exists in write mode without override', async () => {
    const { api } = setupAccept({
      enteredPath: ref(path(['dir'])),
      assetExists: async () => ({ exists: true, type: AssetType.file }),
    })
    await api.tryAcceptCurrentFile()
    expect(api.overwriteFilename.value).toBe('file.txt')
  })

  test("warns when chosen path is a directory (can't accept)", async () => {
    const { api } = setupAccept({
      enteredPath: ref(path(['dir'])),
      fullFilePath: ref('subdir'),
      assetExists: async () => ({ exists: true, type: AssetType.directory }),
    })
    await api.tryAcceptCurrentFile()
    expect(api.warningText.value).toContain('is a directory, not a file')
  })

  test('propagates entering error into warning text', async () => {
    const { api } = setupAccept({ setBrowsingPath: async () => Err('Directory not found') })
    await api.tryAcceptCurrentFile()
    expect(api.warningText.value).toBe('Directory not found')
  })
})
