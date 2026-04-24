import { DirectoryId } from 'enso-common/src/services/Backend'
import { Ok, type Result } from 'enso-common/src/utilities/data/result'
import { uuidv4 } from 'lib0/random'
import { describe, expect, test, vi } from 'vitest'
import { nextTick, ref, type Ref } from 'vue'
import type { EnsoPath } from '../ensoPath'
import { useFileBrowserSync } from '../useFileBrowserSync'

const rootId = DirectoryId(`directory-${uuidv4()}`)
function path(segments: string[] = []): EnsoPath {
  return { root: rootId, segments }
}

describe('useFileBrowserSync', () => {
  function setupSync(options?: {
    writeMode?: Ref<boolean>
    choosenPath?: Ref<string>
    currentDirPath?: Ref<EnsoPath | undefined>
    chosenFilename?: Ref<string | null>
    unenteredPathSuffix?: Ref<string>
    parseEnsoPath?: (p: string) => Result<EnsoPath, string>
    setPath?: (path: Result<EnsoPath>) => void
    setBrowsingPath?: (path: EnsoPath) => Promise<Result<void, any>>
    append?: (name: string) => (segments: string[]) => string[]
    setFilename?: (name: string) => void
  }) {
    const writeMode = options?.writeMode ?? ref(false)
    const choosenPath = options?.choosenPath ?? ref('')
    const currentDirPath = options?.currentDirPath ?? ref(undefined)
    const chosenFilename = options?.chosenFilename ?? ref(null)
    const unenteredPathSuffix = options?.unenteredPathSuffix ?? ref('')

    const parseEnsoPath = options?.parseEnsoPath ?? ((p: string) => Ok(path([p])))
    const setPath = options?.setPath ?? vi.fn()
    const setBrowsingPath = options?.setBrowsingPath ?? vi.fn()
    const append =
      options?.append ?? ((name: string) => (segments: string[]) => [...segments, name])
    const setFilename = options?.setFilename ?? vi.fn()

    useFileBrowserSync({
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
    })

    return {
      writeMode,
      choosenPath,
      currentDirPath,
      chosenFilename,
      unenteredPathSuffix,
      parseEnsoPath,
      setPath,
      setBrowsingPath,
      append,
      setFilename,
    }
  }

  test('syncs setPath from choosenPath', async () => {
    const choosenPath = ref('enso://dir/file')
    const { setPath, parseEnsoPath } = setupSync({
      choosenPath,
      parseEnsoPath: vi.fn((_: string) => Ok(path(['dir', 'file']))),
    })

    await nextTick()
    expect(parseEnsoPath).toHaveBeenCalledWith('enso://dir/file')
    expect(setPath).toHaveBeenCalledTimes(1)

    // Changing the choosenPath should trigger again
    choosenPath.value = 'enso://another'
    await nextTick()
    expect(parseEnsoPath).toHaveBeenCalledWith('enso://another')
    expect(setPath).toHaveBeenCalledTimes(2)
  })

  test('updates browsing path when currentDirPath or chosenFilename changes', async () => {
    const currentDirPath = ref(path(['dir']))
    const chosenFilename = ref('')
    const { setBrowsingPath } = setupSync({
      currentDirPath,
      chosenFilename,
      parseEnsoPath: (p) => Ok(path([p])),
    })

    await nextTick()
    // With only dir set, it should set browsing path to that dir
    expect(setBrowsingPath).toHaveBeenLastCalledWith(path(['dir']))

    // When a filename is chosen, it should append it
    chosenFilename.value = 'file.txt'
    await nextTick()
    expect(setBrowsingPath).toHaveBeenLastCalledWith(path(['dir', 'file.txt']))

    // When the directory changes again, it should recompute
    currentDirPath.value = path(['another'])
    await nextTick()
    expect(setBrowsingPath).toHaveBeenLastCalledWith(path(['another', 'file.txt']))
  })

  test('syncs filename from unenteredPathSuffix only in writeMode', async () => {
    const writeMode = ref(false)
    const unenteredPathSuffix = ref('draft.csv')
    const { setFilename } = setupSync({
      writeMode,
      unenteredPathSuffix,
      parseEnsoPath: (p) => Ok(path([p])),
    })

    await nextTick()
    expect(setFilename).not.toHaveBeenCalled()

    writeMode.value = true
    await nextTick()
    expect(setFilename).toHaveBeenCalledWith('draft.csv')

    // Changing suffix should invoke again
    unenteredPathSuffix.value = 'new.csv'
    await nextTick()
    expect(setFilename).toHaveBeenLastCalledWith('new.csv')
  })

  test('sets filename when chosenFilename is provided', async () => {
    const chosenFilename = ref('picked.txt')
    const { setFilename } = setupSync({ chosenFilename, parseEnsoPath: (p) => Ok(path([p])) })

    await nextTick()
    expect(setFilename).toHaveBeenCalledWith('picked.txt')

    chosenFilename.value = 'another.txt'
    await nextTick()
    expect(setFilename).toHaveBeenLastCalledWith('another.txt')
  })
})
