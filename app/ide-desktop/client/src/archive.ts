import gunzipMaybe from 'gunzip-maybe'
import { Stream } from 'node:stream'
import { createGzip } from 'node:zlib'
import { pack as tarPack } from 'tar-stream'
import { default as ZipStream, type FileDataInput } from 'zip-stream'
gunzipMaybe()

export interface ArchiveEntryMetadata extends FileDataInput {
  readonly name: string
  readonly mode?: number | undefined
}

interface ArchiveBuilderFunction {
  (onCreate: (stream: Stream) => void): ArchiveBuilder
}

export interface ArchiveBuilder {
  readonly stream: Stream
  readonly addFile: (
    source: Buffer | _Readable.Stream | Stream | string,
    data: ArchiveEntryMetadata,
  ) => Promise<void>
  readonly addDirectory: (data: ArchiveEntryMetadata) => Promise<void>
  readonly finalize: () => void
}

zipWriteStream satisfies ArchiveBuilderFunction
/** Create a stream to encode to a `.zip` file. */
export function zipWriteStream(onCreate: (stream: Stream) => void): ArchiveBuilder {
  const archive = new ZipStream()
  onCreate(archive)
  return {
    stream: archive,
    addFile(source, data) {
      return new Promise((resolve, reject) => {
        archive.entry(source, data, (error, entry) => (entry ? resolve() : reject(error)))
      })
    },
    addDirectory(data) {
      return new Promise((resolve, reject) => {
        archive.entry(null, data, (error, entry) => (entry ? resolve() : reject(error)))
      })
    },
    finalize() {
      archive.finish()
    },
  }
}

tarWriteStream satisfies ArchiveBuilderFunction
/** Create a stream to encode to a `.tar` file. */
function tarWriteStream(onCreate: (stream: Stream) => void): ArchiveBuilder {
  const archive = tarPack()
  onCreate(archive)
  return {
    stream: archive,
    addFile(source, data) {
      return new Promise((resolve, reject) => {
        if (Buffer.isBuffer(source) || typeof source === 'string') {
          archive.entry({ type: 'file', ...data }, source, (error) =>
            error ? reject(error) : resolve(),
          )
        } else {
          const entry = archive.entry({ type: 'file', ...data }, (error) =>
            error ? reject(error) : resolve(),
          )
          source.pipe(entry)
        }
      })
    },
    addDirectory(data) {
      return new Promise((resolve) => {
        const entry = archive.entry({ type: 'directory', ...data }, () => {
          resolve()
        })
        entry.end()
      })
    },
    finalize() {
      archive.finalize()
    },
  }
}

tarWriteStream satisfies ArchiveBuilderFunction
/** Create a stream to encode to a `.tar` file. */
export function tarGzWriteStream(onCreate: (stream: Stream) => void): ArchiveBuilder {
  const gzipStream = createGzip()
  const builder = tarWriteStream((stream) => {
    stream.pipe(gzipStream)
    onCreate(gzipStream)
  })
  return { ...builder, stream: gzipStream }
}
