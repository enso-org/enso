import { DataServer } from '@/util/net/dataServer'
import { bytesToHex, Hash } from '@noble/hashes/utils'
import { Error as DataError } from 'ydoc-shared/binaryProtocol'
import { ErrorCode, LanguageServer, RemoteRpcError } from 'ydoc-shared/languageServer'
import { Path, Uuid } from 'ydoc-shared/languageServerTypes'
import { Err, Ok, Result, withContext } from 'ydoc-shared/util/data/result'

export type ProjectFiles = ReturnType<typeof useProjectFiles>

/**
 * A composable with project files operations.
 */
export function useProjectFiles(projectStore: {
  projectRootId: Promise<Uuid | undefined>
  lsRpcConnection: LanguageServer
  dataConnection: DataServer
}) {
  const { projectRootId, lsRpcConnection: lsRpc, dataConnection } = projectStore

  async function readFileBinary(path: Path): Promise<Result<Blob>> {
    const result = await dataConnection.readFile(path)
    if (result instanceof DataError) {
      return Err(result.message() ?? 'Failed to read file.')
    }
    const contents = result.contentsArray()
    if (contents == null) {
      return Err('No file contents received.')
    }
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
    const { stem, extension = '' } = splitFilename(suggestedName)
    let candidate = suggestedName
    let num = 1
    while (existingNames.has(candidate)) {
      candidate = `${stem}_${num}.${extension}`
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

/** Split filename into stem and (optional) extension. */
function splitFilename(fileName: string): { stem: string; extension?: string } {
  const dotIndex = fileName.lastIndexOf('.')
  if (dotIndex !== -1 && dotIndex !== 0) {
    const stem = fileName.substring(0, dotIndex)
    const extension = fileName.substring(dotIndex + 1)
    return { stem, extension }
  }
  return { stem: fileName }
}
