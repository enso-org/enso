import { Awareness } from '@/stores/awareness'
import { ProjectFiles, useProjectFiles } from '@/stores/projectFiles'
import { Vec2 } from '@/util/data/vec2'
import type { DataServer } from '@/util/net/dataServer'
import { Keccak, sha3_224 as SHA3 } from '@noble/hashes/sha3'
import type { Hash } from '@noble/hashes/utils'
import { escapeTextLiteral } from 'ydoc-shared/ast/text'
import type { LanguageServer } from 'ydoc-shared/languageServer'
import type { Path, Uuid } from 'ydoc-shared/languageServerTypes'
import { Err, Ok, type Result } from 'ydoc-shared/util/data/result'
import { type ExternalId } from 'ydoc-shared/yjsModel'

// === Constants ===

const DATA_DIR_NAME = 'data'

/** @returns the expression for node reading an uploaded file. */
export function uploadedExpression(result: UploadResult) {
  switch (result.source) {
    case 'Project': {
      return `enso_project.data/'${escapeTextLiteral(result.name)}' . read`
    }
    case 'FileSystemRoot': {
      return `Data.read '${escapeTextLiteral(result.name)}'`
    }
  }
}

// === Uploader ===

/** Upload result, containing information about upload destination. */
export interface UploadResult {
  source: 'FileSystemRoot' | 'Project'
  name: string
}

/**
 * Uploader handles the uploading process of a single file to project directory.
 *
 * This will upload file chunks using binary protocol, updating information of progress in
 * {@link Awareness} object. On error, the file will be deleted.
 *
 * Checking the checksum is not implemented yet because of https://github.com/enso-org/enso/issues/6691
 */
export class Uploader {
  private checksum: Hash<Keccak>
  private uploadedBytes: bigint
  private awareness: Awareness
  private projectFiles: ProjectFiles

  private constructor(
    projectStore: {
      projectRootId: Promise<Uuid | undefined>
      lsRpcConnection: LanguageServer
      dataConnection: DataServer
      awareness: Awareness
    },
    private file: File,
    private position: Vec2,
    private isOnLocalBackend: boolean,
    private disableDirectRead: boolean,
    private readonly method: ExternalId,
  ) {
    this.checksum = SHA3.create()
    this.uploadedBytes = BigInt(0)
    this.awareness = projectStore.awareness
    this.projectFiles = useProjectFiles(projectStore)
  }

  /** Constructor */
  static Create(
    projectStore: {
      projectRootId: Promise<Uuid | undefined>
      lsRpcConnection: LanguageServer
      dataConnection: DataServer
      awareness: Awareness
    },
    file: File,
    position: Vec2,
    isOnLocalBackend: boolean,
    disableDirectRead: boolean,
    method: ExternalId,
  ): Uploader {
    return new Uploader(projectStore, file, position, isOnLocalBackend, disableDirectRead, method)
  }

  private progressUpdate(sizePercentage: number) {
    return {
      sizePercentage,
      position: this.position,
      method: this.method,
    }
  }

  /** Start the upload process */
  async upload(): Promise<Result<UploadResult>> {
    // This non-standard property is defined in Electron.
    if (
      this.isOnLocalBackend &&
      !this.disableDirectRead &&
      'path' in this.file &&
      typeof this.file.path === 'string'
    ) {
      return Ok({ source: 'FileSystemRoot', name: this.file.path })
    }
    const rootId = await this.projectFiles.projectRootId
    if (rootId == null) return Err('Could not identify project root.')
    const dataDirPath = { rootId, segments: [DATA_DIR_NAME] }
    const dataDirExists = await this.projectFiles.ensureDirExists(dataDirPath)
    if (!dataDirExists.ok) return dataDirExists
    const name = await this.projectFiles.pickUniqueName(dataDirPath, this.file.name)
    if (!name.ok) return name
    this.awareness.addOrUpdateUpload(name.value, this.progressUpdate(0))
    const remotePath: Path = { rootId, segments: [DATA_DIR_NAME, name.value] }
    const cleanup = this.cleanup.bind(this, name.value)
    const writableStream = new WritableStream<Uint8Array>({
      write: async (chunk: Uint8Array) => {
        const result = await this.projectFiles.writeBytes(
          remotePath,
          this.uploadedBytes,
          false,
          chunk,
        )
        if (!result.ok) throw result.error
        this.checksum.update(chunk)
        this.uploadedBytes += BigInt(chunk.length)
        const bytes = Number(this.uploadedBytes)
        const sizePercentage = Math.round((bytes / this.file.size) * 100)
        this.awareness.addOrUpdateUpload(name.value, this.progressUpdate(sizePercentage))
      },
      close: cleanup,
      abort: async (reason: string) => {
        cleanup()
        await this.projectFiles.deleteFile(remotePath)
        throw new Error(`Uploading process aborted. ${reason}`)
      },
    })
    // Disabled until https://github.com/enso-org/enso/issues/6691 is fixed.
    // Plus, handle the error here, as it should be displayed to the user.
    // this.projectFiles.assertChecksum(remotePath)
    await this.file.stream().pipeTo(writableStream)
    return Ok({ source: 'Project', name: name.value })
  }

  private cleanup(name: string) {
    this.awareness.removeUpload(name)
  }
}
