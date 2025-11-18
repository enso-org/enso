import { type ProjectStore } from '$/providers/openedProjects/project'
import { bytesToHex, Hash } from '@noble/hashes/utils'
import { Err, Ok, type Result, withContext } from 'enso-common/src/utilities/data/result'
import { basenameAndExtension } from 'enso-common/src/utilities/file'
import { Error as DataError } from 'ydoc-shared/binaryProtocol'
import { ErrorCode, RemoteRpcError } from 'ydoc-shared/languageServer'
import { type Path } from 'ydoc-shared/languageServerTypes'

export type ProjectFiles = ReturnType<typeof useProjectFiles>

type ProjectStoreSubset = Pick<ProjectStore, 'projectRootId' | 'lsRpcConnection' | 'dataConnection'>

/**
 * A composable with project files operations.
 */
export function useProjectFiles(projectStore: ProjectStoreSubset) {
  const { projectRootId, lsRpcConnection: lsRpc, dataConnection } = projectStore

  async function readFileBinary(path: Path, abort?: AbortSignal): Promise<Result<Blob>> {
    const result = await dataConnection.readFile(path)
    if (abort?.aborted) return Err(abort)
    if (result instanceof DataError) return Err(result.message() ?? 'Failed to read file.')

    const contents = result.contentsArray()
    if (contents == null) return Err('No file contents received.')
    return Ok(new Blob([contents]))
  }

  async function writeFileBinary(path: Path, content: Blob): Promise<Result> {
    const result = await dataConnection.writeFile(path, await content.arrayBuffer())
    if (result instanceof DataError) {
      return Err(result.message() ?? 'Failed to write file.')
    }
    return Ok()
  }

  async function writeBytes(
    path: Path,
    offset: bigint,
    overwriteExisting: boolean,
    contents: string | ArrayBuffer | Uint8Array,
  ): Promise<Result> {
    const result = await dataConnection.writeBytes(path, offset, overwriteExisting, contents)
    if (result instanceof DataError) {
      return Err(result.message() ?? 'Failed to write bytes.')
    }
    return Ok()
  }

  async function deleteFile(path: Path) {
    return lsRpc.deleteFile(path)
  }

  /** Check if directory exists and try to create one if missing. */
  async function ensureDirExists(path: Path): Promise<Result<void>> {
    const exists = await dirExists(path)
    if (!exists.ok) return exists
    if (exists.value) return Ok()

    const name = path.segments.at(-1)
    if (name == null) return Err('Cannot create context root')

    return await withContext(
      () => 'When creating directory for uploaded file',
      async () => {
        return await lsRpc.createFile({
          type: 'Directory',
          name,
          path: { rootId: path.rootId, segments: path.segments.slice(0, -1) },
        })
      },
    )
  }

  /**
   * Check if directory exists. If it does not, or it is a file, `Ok(false)` is returned.
   * In case of error, the directory existence is not confirmed nor disproved.
   */
  async function dirExists(path: Path): Promise<Result<boolean>> {
    const info = await lsRpc.fileInfo(path)
    if (info.ok) return Ok(info.value.attributes.kind.type == 'Directory')
    else if (
      info.error.payload.cause instanceof RemoteRpcError &&
      (info.error.payload.cause.code === ErrorCode.FILE_NOT_FOUND ||
        info.error.payload.cause.code === ErrorCode.CONTENT_ROOT_NOT_FOUND)
    ) {
      return Ok(false)
    } else {
      return info
    }
  }

  /**
   * Return a name for a file which does not collide with existing files in `path`.
   *
   * First choice is `suggestedName`, and then try to apply a numeric suffix to stem.
   */
  async function pickUniqueName(path: Path, suggestedName: string): Promise<Result<string>> {
    const files = await lsRpc.listFiles(path)
    if (!files.ok) return files
    const existingNames = new Set(files.value.paths.map((path) => path.name))
    const { basename, extension = '' } = basenameAndExtension(suggestedName)
    let candidate = suggestedName
    let num = 1
    while (existingNames.has(candidate)) {
      candidate = `${basename}_${num}.${extension}`
      num += 1
    }
    return Ok(candidate)
  }

  async function assertChecksum<T extends Hash<T>>(
    path: Path,
    checksum: Hash<T>,
  ): Promise<Result<void>> {
    const engineChecksum = await lsRpc.fileChecksum(path)
    if (!engineChecksum.ok) return engineChecksum
    const hexChecksum = bytesToHex(checksum.digest())
    if (hexChecksum != engineChecksum.value.checksum) {
      return Err(`Checksum does not match. ${hexChecksum} != ${engineChecksum.value.checksum}`)
    } else {
      return Ok()
    }
  }

  return {
    projectRootId,
    readFileBinary,
    writeFileBinary,
    writeBytes,
    deleteFile,
    ensureDirExists,
    pickUniqueName,
    assertChecksum,
  }
}
