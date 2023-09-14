import * as random from 'lib0/random'
import type { WebsocketClient } from 'lib0/websocket'
import type { MessageEvent } from 'ws'
import {
  Builder,
  ByteBuffer,
  ChecksumBytesCommand,
  ChecksumBytesReply,
  EnsoUUID,
  Error,
  FileContentsReply,
  FileSegment,
  InboundMessage,
  InboundPayload,
  InitSessionCommand,
  None,
  OutboundMessage,
  OutboundPayload,
  ReadBytesCommand,
  ReadBytesReply,
  ReadFileCommand,
  Success,
  Table,
  VisualizationUpdate,
  WriteBytesCommand,
  WriteBytesReply,
  WriteFileCommand,
} from './binaryProtocol'

function uuidFromBits(leastSigBits: bigint, mostSigBits: bigint): string {
  const bits = (mostSigBits << 64n) | leastSigBits
  const string = bits.toString(16).padStart(32, '0')
  return string.replace(/(........)(....)(....)(....)(............)/, '$1-$2-$3-$4-$5')
}

function uuidToBits(uuid: string): [leastSigBits: bigint, mostSigBits: bigint] {
  const bits = BigInt('0x' + uuid.replace(/-/g, ''))
  return [bits & 0xffff_ffff_ffff_ffffn, bits >> 64n]
}

const PAYLOAD_CONSTRUCTOR = {
  [OutboundPayload.NONE]: None,
  [OutboundPayload.ERROR]: Error,
  [OutboundPayload.SUCCESS]: Success,
  [OutboundPayload.VISUALIZATION_UPDATE]: VisualizationUpdate,
  [OutboundPayload.FILE_CONTENTS_REPLY]: FileContentsReply,
  [OutboundPayload.WRITE_BYTES_REPLY]: WriteBytesReply,
  [OutboundPayload.READ_BYTES_REPLY]: ReadBytesReply,
  [OutboundPayload.CHECKSUM_BYTES_REPLY]: ChecksumBytesReply,
} satisfies Record<OutboundPayload, new () => Table>

export class DataServer {
  uuid
  resolveCallbacks = new Map<string, (data: any) => void>()

  /** `websocket.binaryType` should be `ArrayBuffer`. */
  constructor(public websocket: WebsocketClient) {
    this.uuid = random.uuidv4()
    websocket.on('message', (message: MessageEvent) => {
      if (!(message.data instanceof ArrayBuffer)) {
        // Ignore all non-binary messages. If the messages are `Blob`s instead, this is a
        // misconfiguration and should also be ignored.
        return
      }
      const binaryMessage = new OutboundMessage().init(0, new ByteBuffer(message.data))
      const id = binaryMessage.messageId()
      if (id == null) {
        // Invalid id; ignore.
        return
      }
      const uuid = uuidFromBits(id.leastSigBits(), id.mostSigBits())
      const callback = this.resolveCallbacks.get(uuid)
      callback?.(binaryMessage.payload(new PAYLOAD_CONSTRUCTOR[binaryMessage.payloadType()]()))
    })
  }

  protected send<T = void>(
    builder: Builder,
    payloadType: InboundPayload,
    payloadOffset: number,
  ): Promise<T> {
    const messageUuid = random.uuidv4()
    const messageId = this.createUUID(builder, messageUuid)
    const rootTable = InboundMessage.createInboundMessage(
      builder,
      messageId,
      0 /* correlation id */,
      payloadType,
      payloadOffset,
    )
    const promise = new Promise<T>((resolve) => {
      this.resolveCallbacks.set(messageUuid, resolve)
    })
    this.websocket.send(builder.finish(rootTable).toArrayBuffer())
    return promise
  }

  protected createUUID(builder: Builder, uuid: string) {
    const [leastSigBits, mostSigBits] = uuidToBits(uuid)
    const identifier = EnsoUUID.createEnsoUUID(builder, leastSigBits, mostSigBits)
    return identifier
  }

  initSession(): Promise<Success> {
    const builder = new Builder()
    const identifierOffset = this.createUUID(builder, this.uuid)
    const commandOffset = InitSessionCommand.createInitSessionCommand(builder, identifierOffset)
    return this.send<Success>(builder, InboundPayload.INIT_SESSION_CMD, commandOffset)
  }

  writeFile(path: string, contents: string | ArrayBuffer | Uint8Array) {
    const builder = new Builder()
    const contentsOffset = builder.createString(contents)
    const pathOffset = builder.createString(path)
    const command = WriteFileCommand.createWriteFileCommand(builder, pathOffset, contentsOffset)
    return this.send(builder, InboundPayload.WRITE_FILE_CMD, command)
  }

  readFile(path: string) {
    const builder = new Builder()
    const pathOffset = builder.createString(path)
    const command = ReadFileCommand.createReadFileCommand(builder, pathOffset)
    return this.send(builder, InboundPayload.READ_FILE_CMD, command)
  }

  writeBytes(
    path: string,
    index: bigint,
    overwriteExisting: boolean,
    contents: string | ArrayBuffer | Uint8Array,
  ): Promise<WriteBytesReply> {
    const builder = new Builder()
    const bytesOffset = builder.createString(contents)
    const pathOffset = builder.createString(path)
    const command = WriteBytesCommand.createWriteBytesCommand(
      builder,
      pathOffset,
      index,
      overwriteExisting,
      bytesOffset,
    )
    return this.send<WriteBytesReply>(builder, InboundPayload.WRITE_BYTES_CMD, command)
  }

  readBytes(path: string, index: bigint, length: bigint): Promise<ReadBytesReply> {
    const builder = new Builder()
    const pathOffset = builder.createString(path)
    const segmentOffset = FileSegment.createFileSegment(builder, pathOffset, index, length)
    const command = ReadBytesCommand.createReadBytesCommand(builder, segmentOffset)
    return this.send<ReadBytesReply>(builder, InboundPayload.READ_BYTES_CMD, command)
  }

  checksumBytes(path: string, index: bigint, length: bigint): Promise<ChecksumBytesReply> {
    const builder = new Builder()
    const pathOffset = builder.createString(path)
    const segmentOffset = FileSegment.createFileSegment(builder, pathOffset, index, length)
    const command = ChecksumBytesCommand.createChecksumBytesCommand(builder, segmentOffset)
    return this.send<ChecksumBytesReply>(builder, InboundPayload.WRITE_BYTES_CMD, command)
  }

  // TODO: can also send `VisualizationUpdate`
  // TODO: check whether any of these may send an "error" message instead
}
