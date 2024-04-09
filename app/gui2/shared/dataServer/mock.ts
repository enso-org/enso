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
  Null,
  OutboundMessage,
  OutboundPayload,
  Path,
  ReadBytesCommand,
  ReadBytesReply,
  ReadFileCommand,
  Success,
  WriteBytesCommand,
  WriteFileCommand,
  type AnyOutboundPayload,
  type Offset,
  type Table,
} from '../binaryProtocol'
import { LanguageServerErrorCode } from '../languageServerTypes'
import { uuidToBits } from '../uuid'

const sha3 = createSHA3(224)

function pathSegments(path: Path) {
  return Array.from({ length: path.segmentsLength() }, (_, i) => path.segments(i))
}

function createError(builder: Builder, code: LanguageServerErrorCode, message: string) {
  const messageOffset = builder.createString(message)
  return {
    type: OutboundPayload.ERROR,
    offset: ErrorResponse.createError(builder, code, messageOffset, ErrorPayload.NONE, Null),
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

const PAYLOAD_CONSTRUCTOR = {
  [InboundPayload.NONE]: None,
  [InboundPayload.INIT_SESSION_CMD]: InitSessionCommand,
  [InboundPayload.WRITE_FILE_CMD]: WriteFileCommand,
  [InboundPayload.READ_FILE_CMD]: ReadFileCommand,
  [InboundPayload.WRITE_BYTES_CMD]: WriteBytesCommand,
  [InboundPayload.READ_BYTES_CMD]: ReadBytesCommand,
  [InboundPayload.CHECKSUM_BYTES_CMD]: ChecksumBytesCommand,
} satisfies Record<InboundPayload, new () => Table>

export function mockDataWSHandler(
  readFile: (segments: string[]) => Promise<ArrayBuffer | null | undefined>,
  cb?: (send: (data: string | Blob | ArrayBufferLike | ArrayBufferView) => void) => void,
) {
  let sentSend = false
  return async (
    message: string | Blob | ArrayBufferLike | ArrayBufferView,
    send: (data: string | Blob | ArrayBufferLike | ArrayBufferView) => void,
  ) => {
    if (!sentSend) cb?.(send)
    sentSend = true
    if (!(message instanceof ArrayBuffer)) return
    const binaryMessage = InboundMessage.getRootAsInboundMessage(new ByteBuffer(message))
    const payloadType = binaryMessage.payloadType()
    const payload = binaryMessage.payload(new PAYLOAD_CONSTRUCTOR[payloadType]())
    if (!payload) return
    const builder = new Builder()
    let response: { type: OutboundPayload; offset: Offset<AnyOutboundPayload> } | undefined
    switch (payloadType) {
      case InboundPayload.NONE: {
        response = {
          type: OutboundPayload.NONE,
          offset: Null,
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
          'Cannot WriteFile to a read-only mock.',
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
        const file = await readFile(pathSegments(path))
        if (!file) {
          response = createError(builder, LanguageServerErrorCode.FileNotFound, 'File not found')
          break
        }
        const contentOffset = builder.createString(file)
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
          'Cannot WriteBytes to a read-only mock.',
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
        const file = await readFile(pathSegments(path))
        if (!file) {
          response = createError(builder, LanguageServerErrorCode.FileNotFound, 'File not found')
          break
        }
        const start = Number(segment.byteOffset())
        const slice = file.slice(start, start + Number(segment.length()))
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
        const file = await readFile(pathSegments(path))
        if (!file) {
          response = createError(builder, LanguageServerErrorCode.FileNotFound, 'File not found')
          break
        }
        const start = Number(segment.byteOffset())
        const slice = file.slice(start, start + Number(segment.length()))
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
  }
}
