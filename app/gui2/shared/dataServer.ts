import { ObservableV2 } from 'lib0/observable'
import * as random from 'lib0/random'
import type { Path as LSPath } from 'shared/languageServerTypes'
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
  Path,
  ReadBytesCommand,
  ReadBytesReply,
  ReadFileCommand,
  Success,
  VisualizationUpdate,
  WriteBytesCommand,
  WriteBytesReply,
  WriteFileCommand,
  type AnyInboundPayload,
  type Offset,
  type Table,
} from './binaryProtocol'
import { uuidFromBits, uuidToBits } from './uuid'
import type { WebsocketClient } from './websocket'
import type { Uuid } from './yjsModel'

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

export type DataServerEvents = {
  [K in keyof typeof PAYLOAD_CONSTRUCTOR as `${K}`]: (
    payload: InstanceType<(typeof PAYLOAD_CONSTRUCTOR)[K]>,
    uuid: Uuid | null,
  ) => void
}

export class DataServer extends ObservableV2<DataServerEvents> {
  initialized = false
  ready: Promise<void>
  clientId!: string
  resolveCallbacks = new Map<string, (data: any) => void>()

  /** `websocket.binaryType` should be `ArrayBuffer`. */
  constructor(public websocket: WebsocketClient) {
    super()
    if (websocket.connected) {
      this.ready = Promise.resolve()
    } else {
      this.ready = new Promise((resolve, reject) => {
        websocket.on('connect', () => resolve())
        websocket.on('disconnect', reject)
      })
    }
    websocket.on('message', (rawPayload) => {
      if (!(rawPayload instanceof ArrayBuffer)) {
        console.warn('Data Server: Data type was invalid:', rawPayload)
        // Ignore all non-binary messages. If the messages are `Blob`s instead, this is a
        // misconfiguration and should also be ignored.
        return
      }
      const binaryMessage = OutboundMessage.getRootAsOutboundMessage(new ByteBuffer(rawPayload))
      const payloadType = binaryMessage.payloadType()
      const payload = binaryMessage.payload(new PAYLOAD_CONSTRUCTOR[payloadType]())
      if (!payload) return
      this.emit(`${payloadType}`, [payload, null])
      const id = binaryMessage.correlationId()
      if (id != null) {
        const uuid = uuidFromBits(id.leastSigBits(), id.mostSigBits())
        this.emit(`${payloadType}`, [payload, uuid])
        const callback = this.resolveCallbacks.get(uuid)
        callback?.(payload)
      } else if (payload instanceof VisualizationUpdate) {
        const id = payload.visualizationContext()?.visualizationId()
        if (!id) return
        const uuid = uuidFromBits(id.leastSigBits(), id.mostSigBits())
        this.emit(`${payloadType}`, [payload, uuid])
      }
    })
  }

  async initialize(clientId: Uuid) {
    if (!this.initialized) {
      this.clientId = clientId
      await this.ready
      await this.initSession()
    }
  }

  protected send<T = void>(
    builder: Builder,
    payloadType: InboundPayload,
    payloadOffset: Offset<AnyInboundPayload>,
  ): Promise<T> {
    const messageUuid = random.uuidv4()
    const rootTable = InboundMessage.createInboundMessage(
      builder,
      this.createUUID(messageUuid),
      null /* correlation id */,
      payloadType,
      payloadOffset,
    )
    const promise = new Promise<T>((resolve) => {
      this.resolveCallbacks.set(messageUuid, resolve)
    })
    this.websocket.send(builder.finish(rootTable).toArrayBuffer())
    return promise
  }

  protected createUUID(uuid: string) {
    const [leastSigBits, mostSigBits] = uuidToBits(uuid)
    return (builder: Builder) => EnsoUUID.createEnsoUUID(builder, leastSigBits, mostSigBits)
  }

  initSession(): Promise<Success | Error> {
    const builder = new Builder()
    const commandOffset = InitSessionCommand.createInitSessionCommand(
      builder,
      this.createUUID(this.clientId),
    )
    return this.send(builder, InboundPayload.INIT_SESSION_CMD, commandOffset)
  }

  async writeFile(
    path: LSPath,
    contents: string | ArrayBuffer | Uint8Array,
  ): Promise<Success | Error> {
    const builder = new Builder()
    const contentsOffset = builder.createString(contents)
    const segmentOffsets = path.segments.map((segment) => builder.createString(segment))
    const segmentsOffset = Path.createSegmentsVector(builder, segmentOffsets)
    const pathOffset = Path.createPath(builder, this.createUUID(path.rootId), segmentsOffset)
    const command = WriteFileCommand.createWriteFileCommand(builder, pathOffset, contentsOffset)
    return await this.send(builder, InboundPayload.WRITE_FILE_CMD, command)
  }

  async readFile(path: LSPath): Promise<FileContentsReply | Error> {
    const builder = new Builder()
    const segmentOffsets = path.segments.map((segment) => builder.createString(segment))
    const segmentsOffset = Path.createSegmentsVector(builder, segmentOffsets)
    const pathOffset = Path.createPath(builder, this.createUUID(path.rootId), segmentsOffset)
    const command = ReadFileCommand.createReadFileCommand(builder, pathOffset)
    return await this.send(builder, InboundPayload.READ_FILE_CMD, command)
  }

  async writeBytes(
    path: LSPath,
    index: bigint,
    overwriteExisting: boolean,
    contents: string | ArrayBuffer | Uint8Array,
  ): Promise<WriteBytesReply | Error> {
    const builder = new Builder()
    const bytesOffset = builder.createString(contents)
    const segmentOffsets = path.segments.map((segment) => builder.createString(segment))
    const segmentsOffset = Path.createSegmentsVector(builder, segmentOffsets)
    const pathOffset = Path.createPath(builder, this.createUUID(path.rootId), segmentsOffset)
    const command = WriteBytesCommand.createWriteBytesCommand(
      builder,
      pathOffset,
      index,
      overwriteExisting,
      bytesOffset,
    )
    return await this.send(builder, InboundPayload.WRITE_BYTES_CMD, command)
  }

  async readBytes(path: LSPath, index: bigint, length: bigint): Promise<ReadBytesReply | Error> {
    const builder = new Builder()
    const segmentOffsets = path.segments.map((segment) => builder.createString(segment))
    const segmentsOffset = Path.createSegmentsVector(builder, segmentOffsets)
    const pathOffset = Path.createPath(builder, this.createUUID(path.rootId), segmentsOffset)
    const segmentOffset = FileSegment.createFileSegment(builder, pathOffset, index, length)
    const command = ReadBytesCommand.createReadBytesCommand(builder, segmentOffset)
    return await this.send(builder, InboundPayload.READ_BYTES_CMD, command)
  }

  async checksumBytes(
    path: LSPath,
    index: bigint,
    length: bigint,
  ): Promise<ChecksumBytesReply | Error> {
    const builder = new Builder()
    const segmentOffsets = path.segments.map((segment) => builder.createString(segment))
    const segmentsOffset = Path.createSegmentsVector(builder, segmentOffsets)
    const pathOffset = Path.createPath(builder, this.createUUID(path.rootId), segmentsOffset)
    const segmentOffset = FileSegment.createFileSegment(builder, pathOffset, index, length)
    const command = ChecksumBytesCommand.createChecksumBytesCommand(builder, segmentOffset)
    return await this.send(builder, InboundPayload.WRITE_BYTES_CMD, command)
  }
}
