import { Keccak, sha3_224 as SHA3 } from '@noble/hashes/sha3'
import type { Hash } from '@noble/hashes/utils'
import { bytesToHex } from '@noble/hashes/utils'
import type { DataServer } from 'shared/dataServer'
import type { LanguageServer } from 'shared/languageServer'
import { ErrorCode, RemoteRpcError } from 'shared/languageServer'
import type { ContentRoot, Path, Uuid } from 'shared/languageServerTypes'

export const uploadedExpression = (name: string) => `enso_project.data/"${name}" . read`
const DATA_DIR_NAME = 'data'

export class Uploader {
  private rpc: LanguageServer
  private binary: DataServer
  private file: File
  private projectRootId: Uuid
  private checksum: Hash<Keccak>
  private uploadedBytes: bigint

  private constructor(rpc: LanguageServer, binary: DataServer, file: File, projectRootId: Uuid) {
    this.rpc = rpc
    this.binary = binary
    this.file = file
    this.projectRootId = projectRootId
    this.checksum = SHA3.create()
    this.uploadedBytes = BigInt(0)
  }

  static async create(
    rpc: Promise<LanguageServer>,
    binary: Promise<DataServer>,
    contentRoots: Promise<ContentRoot[]>,
    file: File,
  ): Promise<Uploader> {
    const projectRootId = await contentRoots.then((roots) =>
      roots.find((root) => root.type == 'Project'),
    )
    if (!projectRootId) throw new Error('Unable to find project root, uploading not possible.')
    const instance = new Uploader(await rpc, await binary, file, projectRootId.id)
    return instance
  }

  async upload(): Promise<string> {
    await this.ensureDataDirExists()
    const name = await this.pickUniqueName(this.file.name)
    const remotePath: Path = { rootId: this.projectRootId, segments: [DATA_DIR_NAME, name] }
    const uploader = this
    const writableStream = new WritableStream<Uint8Array>({
      async write(chunk: Uint8Array) {
        await uploader.binary.writeBytes(remotePath, uploader.uploadedBytes, false, chunk)
        uploader.checksum.update(chunk)
        uploader.uploadedBytes += BigInt(chunk.length)
      },
      async close() {
        // Disabled until https://github.com/enso-org/enso/issues/6691 is fixed.
        // uploader.assertChecksum(remotePath)
      },
      async abort(reason: string) {
        await uploader.rpc.deleteFile(remotePath)
        throw new Error(`Uploading process aborted. ${reason}`)
      },
    })
    await this.file.stream().pipeTo(writableStream)
    return name
  }

  private async assertChecksum(path: Path) {
    const engineChecksum = await this.rpc.fileChecksum(path)
    const hexChecksum = bytesToHex(this.checksum.digest())
    if (hexChecksum != engineChecksum.checksum) {
      throw new Error(
        `Uploading file failed, checksum does not match. ${hexChecksum} != ${engineChecksum.checksum}`,
      )
    }
  }

  private dataDirPath(): Path {
    return { rootId: this.projectRootId, segments: [DATA_DIR_NAME] }
  }

  private async ensureDataDirExists() {
    const exists = await this.dataDirExists()
    if (!exists) {
      await this.rpc.createFile({
        type: 'Directory',
        name: DATA_DIR_NAME,
        path: { rootId: this.projectRootId, segments: [] },
      })
    }
  }

  private async dataDirExists(): Promise<boolean> {
    try {
      const info = await this.rpc.fileInfo(this.dataDirPath())
      return info.attributes.kind.type == 'Directory'
    } catch (err: any) {
      if (err.cause && err.cause instanceof RemoteRpcError) {
        if ([ErrorCode.FILE_NOT_FOUND, ErrorCode.CONTENT_ROOT_NOT_FOUND].includes(err.cause.code)) {
          return false
        }
      }
      throw err
    }
  }

  private async pickUniqueName(suggestedName: string): Promise<string> {
    const files = await this.rpc.listFiles(this.dataDirPath())
    const existingNames = new Set(files.paths.map((path) => path.name))
    const [stem, maybeExtension] = splitFilename(suggestedName)
    const extension = maybeExtension ?? ''
    let candidate = suggestedName
    let num = 1
    while (existingNames.has(candidate)) {
      candidate = `${stem}_${num}.${extension}`
      num += 1
    }
    return candidate
  }
}

/**
 * Split filename into stem and (optional) extension.
 */
function splitFilename(filename: string): [string, string | null] {
  const dotIndex = filename.lastIndexOf('.')

  if (dotIndex !== -1 && dotIndex !== 0) {
    const stem = filename.substring(0, dotIndex)
    const extension = filename.substring(dotIndex + 1)
    return [stem, extension]
  }

  return [filename, null]
}
