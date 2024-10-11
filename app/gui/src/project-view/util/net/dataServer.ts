import { ObservableV2 } from 'lib0/observable'
import * as random from 'lib0/random'
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
} from 'ydoc-shared/binaryProtocol'
import type { Path as LSPath } from 'ydoc-shared/languageServerTypes'
import { Err, Ok, type Result } from 'ydoc-shared/util/data/result'
import { exponentialBackoff, type AbortScope } from 'ydoc-shared/util/net'
import { uuidFromBits, uuidToBits } from 'ydoc-shared/uuid'
import type { Uuid } from 'ydoc-shared/yjsModel'

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

/** TODO: Add docs */
export class DataServer extends ObservableV2<DataServerEvents> {
  initialized: Promise<Result<void, Error>>
  private initializationScheduled = false
  resolveCallbacks = new Map<string, (data: any) => void>()

  /** `websocket.binaryType` should be `ArrayBuffer`. */
  constructor(
    public clientId: string,
    public websocket: WebSocket,
    abort: AbortScope,
  ) {
    super()
    abort.handleDispose(this)

    websocket.addEventListener('message', ({ data: rawPayload }) => {
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
    websocket.addEventListener('error', (error) =>
      console.error('Language Server Binary socket error:', error),
    )
    websocket.addEventListener('close', () => {
      this.scheduleInitializationAfterConnect()
    })

    if (websocket.readyState === WebSocket.OPEN) this.initialized = this.initialize()
    else this.initialized = this.scheduleInitializationAfterConnect()
  }

  /** TODO: Add docs */
  dispose() {
    this.websocket.close()
    this.resolveCallbacks.clear()
  }

  private scheduleInitializationAfterConnect() {
    if (this.initializationScheduled) return this.initialized
    this.initializationScheduled = true
    this.initialized = new Promise((resolve) => {
      const cb = () => {
        this.websocket.removeEventListener('open', cb)
        this.initializationScheduled = false
        resolve(this.initialize())
      }
      this.websocket.addEventListener('open', cb)
    })
    return this.initialized
  }

  private async initialize() {
    const result = await exponentialBackoff(() => this.initSession().then(responseAsResult), {
      onBeforeRetry: (error, _, delay) => {
        console.warn(
          `Failed to initialize language server binary connection, retrying after ${delay}ms...\n`,
          error,
        )
      },
    })
    if (!result.ok) {
      result.error.log('Error initializing Language Server Binary Protocol')
      return result
    } else {
      return Ok()
    }
  }

  protected async send<T = void>(
    builder: Builder,
    payloadType: InboundPayload,
    payloadOffset: Offset<AnyInboundPayload>,
    waitForInit: boolean = true,
  ): Promise<T | Error> {
    const messageUuid = random.uuidv4()
    const rootTable = InboundMessage.createInboundMessage(
      builder,
      this.createUUID(messageUuid),
      null /* correlation id */,
      payloadType,
      payloadOffset,
    )
    if (waitForInit) {
      const initResult = await this.initialized
      if (!initResult.ok) {
        return initResult.error.payload
      }
    }
    this.websocket.send(builder.finish(rootTable).toArrayBuffer())
    const promise = new Promise<T | Error>((resolve) => {
      this.resolveCallbacks.set(messageUuid, resolve)
    })
    return promise
  }

  protected createUUID(uuid: string) {
    const [leastSigBits, mostSigBits] = uuidToBits(uuid)
    return (builder: Builder) => EnsoUUID.createEnsoUUID(builder, leastSigBits, mostSigBits)
  }

  /** TODO: Add docs */
  initSession(): Promise<Success | Error> {
    const builder = new Builder()
    const commandOffset = InitSessionCommand.createInitSessionCommand(
      builder,
      this.createUUID(this.clientId),
    )
    return this.send(builder, InboundPayload.INIT_SESSION_CMD, commandOffset, false)
  }

  /** TODO: Add docs */
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

  /** TODO: Add docs */
  async readFile(path: LSPath): Promise<FileContentsReply | Error> {
    const builder = new Builder()
    const segmentOffsets = path.segments.map((segment) => builder.createString(segment))
    const segmentsOffset = Path.createSegmentsVector(builder, segmentOffsets)
    const pathOffset = Path.createPath(builder, this.createUUID(path.rootId), segmentsOffset)
    const command = ReadFileCommand.createReadFileCommand(builder, pathOffset)
    return await this.send(builder, InboundPayload.READ_FILE_CMD, command)
  }

  /** TODO: Add docs */
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

  /** TODO: Add docs */
  async readBytes(path: LSPath, index: bigint, length: bigint): Promise<ReadBytesReply | Error> {
    const builder = new Builder()
    const segmentOffsets = path.segments.map((segment) => builder.createString(segment))
    const segmentsOffset = Path.createSegmentsVector(builder, segmentOffsets)
    const pathOffset = Path.createPath(builder, this.createUUID(path.rootId), segmentsOffset)
    const segmentOffset = FileSegment.createFileSegment(builder, pathOffset, index, length)
    const command = ReadBytesCommand.createReadBytesCommand(builder, segmentOffset)
    return await this.send(builder, InboundPayload.READ_BYTES_CMD, command)
  }

  /** TODO: Add docs */
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

function responseAsResult<T>(resp: T | Error): Result<T, Error> {
  if (resp instanceof Error) return Err(resp)
  else return Ok(resp)
}
