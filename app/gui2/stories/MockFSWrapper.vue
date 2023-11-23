<script setup lang="ts">
import { useProjectStore } from '@/stores/project'
import { mockFsDirectoryHandle } from '@/util/convert/fsAccess'
import { MockWebSocket, type WebSocketHandler } from '@/util/net'
import { createSHA3 } from 'hash-wasm'
import * as random from 'lib0/random'
import {
  Builder,
  ByteBuffer,
  ChecksumBytesCommand,
  ChecksumBytesReply,
  EnsoDigest,
  EnsoUUID,
  ErrorPayload,
  Error as ErrorResponse,
  FileContentsReply,
  InboundMessage,
  InboundPayload,
  InitSessionCommand,
  None,
  OutboundMessage,
  OutboundPayload,
  Path,
  ReadBytesCommand,
  ReadBytesReply,
  ReadFileCommand,
  Success,
  WriteBytesCommand,
  WriteFileCommand,
  type Table,
} from 'shared/binaryProtocol'
import { LanguageServerErrorCode, type Path as LSPath } from 'shared/languageServerTypes'
import { uuidToBits } from 'shared/uuid'
import { watchEffect } from 'vue'

const projectStore = useProjectStore()

interface FileTree {
  [name: string]: FileTree | string | ArrayBuffer
}

const props = defineProps<{
  files: FileTree | undefined
  directory: FileSystemDirectoryHandle | undefined
  /** The path of the root directory. */
  prefix?: string[] | undefined
}>()

const PAYLOAD_CONSTRUCTOR = {
  [InboundPayload.NONE]: None,
  [InboundPayload.INIT_SESSION_CMD]: InitSessionCommand,
  [InboundPayload.WRITE_FILE_CMD]: WriteFileCommand,
  [InboundPayload.READ_FILE_CMD]: ReadFileCommand,
  [InboundPayload.WRITE_BYTES_CMD]: WriteBytesCommand,
  [InboundPayload.READ_BYTES_CMD]: ReadBytesCommand,
  [InboundPayload.CHECKSUM_BYTES_CMD]: ChecksumBytesCommand,
} satisfies Record<InboundPayload, new () => Table>

const sha3 = createSHA3(224)

function pathSegments(path: Path) {
  return Array.from({ length: path.segmentsLength() }, (_, i) => path.segments(i))
}

async function readFile(dir: FileSystemDirectoryHandle, segments: string[]) {
  if (!segments.length) return
  try {
    for (const segment of segments.slice(0, -1)) {
      dir = await dir.getDirectoryHandle(segment)
    }
    const file = await dir.getFileHandle(segments.at(-1)!)
    return await file.getFile()
  } catch {
    return
  }
}

function createError(builder: Builder, code: LanguageServerErrorCode, message: string) {
  const messageOffset = builder.createString(message)
  return {
    type: OutboundPayload.ERROR,
    offset: ErrorResponse.createError(builder, code, messageOffset, ErrorPayload.NONE, 0),
  }
}

function createMessageId(builder: Builder) {
  const messageUuid = random.uuidv4()
  const [leastSigBits, mostSigBits] = uuidToBits(messageUuid)
  return EnsoUUID.createEnsoUUID(builder, leastSigBits, mostSigBits)
}

function createCorrelationId(messageId: EnsoUUID) {
  return (builder: Builder) =>
    EnsoUUID.createEnsoUUID(builder, messageId.leastSigBits(), messageId.mostSigBits())
}

let resolveDataWsHandler: ((handler: WebSocketHandler) => void) | undefined
let dataWsHandler: Promise<WebSocketHandler> = new Promise((resolve) => {
  resolveDataWsHandler = resolve
})
function setDataWsHandler(handler: WebSocketHandler) {
  if (resolveDataWsHandler) {
    resolveDataWsHandler(handler)
    resolveDataWsHandler = undefined
  } else {
    dataWsHandler = Promise.resolve(handler)
  }
}

MockWebSocket.addMock('data', async (data, send) => {
  ;(await dataWsHandler)(data, send)
})

watchEffect(async (onCleanup) => {
  let maybeDirectory = props.files ? mockFsDirectoryHandle(props.files, '(root)') : props.directory
  if (!maybeDirectory) return
  const prefixLength = props.prefix?.length ?? 0
  const directory = maybeDirectory
  const ls = await projectStore.lsRpcConnection
  const maybeProjectRoot = (await projectStore.contentRoots).find((root) => root.type === 'Project')
    ?.id
  if (!maybeProjectRoot) return
  const projectRoot = maybeProjectRoot
  async function walkFiles(
    dir: FileSystemDirectoryHandle,
    segments: string[],
    cb: (path: LSPath) => void,
  ) {
    for await (const [name, dirOrFile] of dir.entries()) {
      const newSegments = [...segments, name]
      if (dirOrFile.kind === 'directory') walkFiles(dirOrFile, newSegments, cb)
      else {
        cb({
          rootId: projectRoot,
          segments: newSegments.slice(prefixLength),
        })
      }
    }
  }
  walkFiles(directory, props.prefix ?? [], (path) =>
    ls.emit('file/event', [{ kind: 'Added', path }]),
  )
  onCleanup(() => {
    walkFiles(directory, props.prefix ?? [], (path) =>
      ls.emit('file/event', [{ kind: 'Removed', path }]),
    )
  })
  setDataWsHandler(async (message, send) => {
    if (!(message instanceof ArrayBuffer)) return
    const binaryMessage = InboundMessage.getRootAsInboundMessage(new ByteBuffer(message))
    const payloadType = binaryMessage.payloadType()
    const payload = binaryMessage.payload(new PAYLOAD_CONSTRUCTOR[payloadType]())
    if (!payload) return
    const builder = new Builder()
    let response: { type: OutboundPayload; offset: number } | undefined
    switch (payloadType) {
      case InboundPayload.NONE: {
        response = {
          type: OutboundPayload.NONE,
          offset: 0,
        }
        break
      }
      case InboundPayload.INIT_SESSION_CMD: {
        response = {
          type: OutboundPayload.SUCCESS,
          offset: Success.createSuccess(builder),
        }
        break
      }
      case InboundPayload.WRITE_FILE_CMD: {
        response = createError(
          builder,
          LanguageServerErrorCode.AccessDenied,
          'Cannot write to a read-only mock.',
        )
        break
      }
      case InboundPayload.READ_FILE_CMD: {
        const payload_ = payload as ReadFileCommand
        const path = payload_.path()
        if (!path) {
          response = createError(builder, LanguageServerErrorCode.NotFile, 'Invalid Path')
          break
        }
        const file = await readFile(directory, pathSegments(path).slice(prefixLength))
        if (!file) {
          response = createError(builder, LanguageServerErrorCode.FileNotFound, 'File not found')
          break
        }
        const contentOffset = builder.createString(await file.arrayBuffer())
        response = {
          type: OutboundPayload.FILE_CONTENTS_REPLY,
          offset: FileContentsReply.createFileContentsReply(builder, contentOffset),
        }
        break
      }
      case InboundPayload.WRITE_BYTES_CMD: {
        response = createError(
          builder,
          LanguageServerErrorCode.AccessDenied,
          'Cannot write to a read-only mock.',
        )
        break
      }
      case InboundPayload.READ_BYTES_CMD: {
        const payload_ = payload as ReadBytesCommand
        const segment = payload_.segment()
        const path = segment && segment.path()
        if (!path) {
          response = createError(
            builder,
            LanguageServerErrorCode.NotFile,
            'Invalid FileSegment or Path',
          )
          break
        }
        const file = await readFile(directory, pathSegments(path).slice(prefixLength))
        if (!file) {
          response = createError(builder, LanguageServerErrorCode.FileNotFound, 'File not found')
          break
        }
        const start = Number(segment.byteOffset())
        const slice = await file.slice(start, start + Number(segment.length())).arrayBuffer()
        const contentOffset = builder.createString(slice)
        const digest = (await sha3).init().update(new Uint8Array(slice)).digest('binary')
        const checksumBytesOffset = EnsoDigest.createBytesVector(builder, digest)
        const checksumOffset = EnsoDigest.createEnsoDigest(builder, checksumBytesOffset)
        response = {
          type: OutboundPayload.READ_BYTES_REPLY,
          offset: ReadBytesReply.createReadBytesReply(builder, checksumOffset, contentOffset),
        }
        break
      }
      case InboundPayload.CHECKSUM_BYTES_CMD: {
        const payload_ = payload as ChecksumBytesCommand
        const segment = payload_.segment()
        const path = segment && segment.path()
        if (!path) {
          response = createError(
            builder,
            LanguageServerErrorCode.NotFile,
            'Invalid FileSegment or Path',
          )
          break
        }
        const file = await readFile(directory, pathSegments(path).slice(prefixLength))
        if (!file) {
          response = createError(builder, LanguageServerErrorCode.FileNotFound, 'File not found')
          break
        }
        const start = Number(segment.byteOffset())
        const slice = await file.slice(start, start + Number(segment.length())).arrayBuffer()
        const digest = (await sha3).init().update(new Uint8Array(slice)).digest('binary')
        const bytesOffset = EnsoDigest.createBytesVector(builder, digest)
        const checksumOffset = EnsoDigest.createEnsoDigest(builder, bytesOffset)
        response = {
          type: OutboundPayload.CHECKSUM_BYTES_REPLY,
          offset: ChecksumBytesReply.createChecksumBytesReply(builder, checksumOffset),
        }
        break
      }
    }
    if (!response) return
    const correlationUuid = binaryMessage.messageId()
    if (!correlationUuid) return
    const rootTable = OutboundMessage.createOutboundMessage(
      builder,
      createMessageId,
      createCorrelationId(correlationUuid),
      response.type,
      response.offset,
    )
    send(builder.finish(rootTable).toArrayBuffer())
  })
})
</script>

<template>
  <slot></slot>
</template>
