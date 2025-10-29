import gunzipMaybe from 'gunzip-maybe'
import { createReadStream, createWriteStream } from 'node:fs'
import { mkdir } from 'node:fs/promises'
import { platform } from 'node:os'
import { dirname, join } from 'node:path'
import type { Readable, Stream, Writable } from 'node:stream'
import { createGzip } from 'node:zlib'
import { extract as tarFsExtract } from 'tar-fs'
import { extract as tarExtract, pack as tarPack } from 'tar-stream'
import { Entry, open as yauzlOpen, type ZipFile } from 'yauzl'
import { default as ZipStream, type FileDataInput } from 'zip-stream'
export { pack as tarFsPack } from 'tar-fs'

export interface ArchiveEntryMetadata extends FileDataInput {
  readonly name: string
  readonly mode?: number | undefined
}

export interface ArchiveBuilder {
  readonly stream: Stream
  readonly addFile: (
    source: Buffer | _Readable.Stream | Stream | string,
    data: ArchiveEntryMetadata,
  ) => Promise<void>
  readonly addFolder: (data: ArchiveEntryMetadata) => Promise<void>
  readonly finalize: () => void
}

/** Convert a readable stream to a promise. */
function readableStreamToPromise(stream: Readable) {
  return new Promise<void>((resolve, reject) => {
    stream.once('end', resolve)
    stream.once('error', reject)
  })
}

/** Convert a stream to a promise. */
function writableStreamToPromise(stream: Writable) {
  return new Promise<void>((resolve, reject) => {
    stream.once('close', resolve)
    stream.once('error', reject)
  })
}

/** Create a stream to encode to a `.zip` file. */
export function zipWriteStream(): ArchiveBuilder {
  const archive = new ZipStream()
  return {
    stream: archive,
    addFile(source, data) {
      return new Promise((resolve, reject) => {
        archive.entry(source, data, (error, entry) => (entry ? resolve() : reject(error)))
      })
    },
    addFolder(data) {
      return new Promise((resolve, reject) => {
        const nameRaw = data.name
        const nameHasTrailingFolderSeparator =
          nameRaw.endsWith('/') || (platform() === 'win32' && nameRaw.endsWith('\\'))
        const name = nameHasTrailingFolderSeparator ? nameRaw : `${nameRaw}/`
        archive.entry(null, { ...data, name }, (error, entry) =>
          entry ? resolve() : reject(error),
        )
      })
    },
    finalize() {
      archive.finish()
    },
  }
}

/** Create a stream to encode to a `.tar` file. */
function tarWriteStream(): ArchiveBuilder {
  const archive = tarPack()
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
    addFolder(data) {
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

/** Create a stream to encode to a `.tar` file. */
export function tarGzWriteStream(): ArchiveBuilder {
  const builder = tarWriteStream()
  return { ...builder, stream: builder.stream.pipe(createGzip()) }
}

/** Exrtract a `.tar` file to the filesystem. */
async function tarReadStreamToFs(stream: Stream, path: string) {
  return writableStreamToPromise(stream.pipe(tarFsExtract(path)))
}

/** Exrtract a `.tar.gz` file to the filesystem. */
export async function tarGzReadStreamToFs(stream: Stream, path: string) {
  await tarReadStreamToFs(stream.pipe(gunzipMaybe()), path)
}

export interface UnpackEntryMetadata {
  readonly name: string
}

export interface ArchiveEntry {
  readonly metadata: UnpackEntryMetadata
  readonly getDestinationPath: (rootDirectory: string) => string
  /**
   * `rootDirectory` is the root directory of extraction. This will be joined with the path
   * in the archive to form the destination path.
   */
  readonly extract: (options: {
    rootDirectory: string
    destinationPath?: string
    transform?: (
      stream: Readable,
      entry: UnpackEntryMetadata,
    ) => Promise<Readable | null | undefined | false> | Readable | null | undefined | false
  }) => Promise<void>
}

/** Return an async iterator over the entries of a `.zip` file. */
export async function unzipEntries(path: string) {
  const archive = await new Promise<ZipFile>((resolve, reject) => {
    yauzlOpen(path, { lazyEntries: true }, (error, archive) =>
      error ? reject(error) : resolve(archive),
    )
  })
  const skipped: Record<string, boolean> = {}
  return {
    async *[Symbol.asyncIterator]() {
      while (true) {
        const promise = new Promise<ArchiveEntry | null>((resolve, reject) => {
          const end = () => {
            resolve(null)
          }
          archive.on('error', reject)
          archive.on('end', end)
          archive.on('entry', onEntry)
          function onEntry(entry: Entry) {
            const entryIsDirectory = /[/\\]$/.test(entry.fileName)
            if (skipped[dirname(entry.fileName)] == true) {
              if (entryIsDirectory) {
                skipped[entry.fileName] = true
              }
              return
            }
            const archiveEntry: ArchiveEntry = {
              metadata: { name: entry.fileName },
              getDestinationPath(rootDirectory) {
                return join(rootDirectory, entry.fileName)
              },
              async extract({ rootDirectory, destinationPath, transform }) {
                destinationPath ??= archiveEntry.getDestinationPath(rootDirectory)
                if (entryIsDirectory) {
                  await mkdir(destinationPath, { recursive: true })
                  return
                }
                // According to `yauzl` documentation:
                // Entries for directories themselves are optional in `.zip` archives.
                await mkdir(dirname(destinationPath), { recursive: true })
                const readStreamRaw = await new Promise<Readable>((resolve, reject) =>
                  archive.openReadStream(entry, (error, readStream) =>
                    error ? reject(error) : resolve(readStream),
                  ),
                )
                const readStream =
                  (await transform?.(readStreamRaw, archiveEntry.metadata)) ?? readStreamRaw
                if (readStream === false) {
                  // Skip this entry.
                  return
                }
                const promise = readableStreamToPromise(readStream)
                readStream.pipe(createWriteStream(destinationPath))
                return await promise
              },
            }
            archive.off('error', reject)
            archive.off('end', end)
            archive.off('entry', onEntry)
            resolve(archiveEntry)
          }
        })
        archive.readEntry()
        const result = await promise
        if (!result) {
          break
        }
        yield result
      }
    },
  }
}

/** Return an async iterator over the entries of a `.tar.gz` file. */
export async function untarGzEntries(path: string) {
  const archive = tarExtract()
  const result = {
    async *[Symbol.asyncIterator]() {
      for await (const file of archive) {
        const entry = file.header
        const archiveEntry: ArchiveEntry = {
          metadata: { name: entry.name },
          getDestinationPath(rootDirectory) {
            return join(rootDirectory, entry.name)
          },
          async extract({ rootDirectory, destinationPath, transform }) {
            destinationPath ??= archiveEntry.getDestinationPath(rootDirectory)
            if (entry.type === 'directory') {
              await mkdir(destinationPath, { mode: entry.mode })
              return
            }
            const readStream = (await transform?.(file, archiveEntry.metadata)) ?? file
            if (readStream === false) {
              // Skip this entry.
              return
            }
            const promise = readableStreamToPromise(readStream)
            readStream.pipe(createWriteStream(destinationPath, { mode: entry.mode }))
            return await promise
          },
        }
        yield archiveEntry
      }
    },
  }
  createReadStream(path).pipe(gunzipMaybe()).pipe(archive)
  return result
}
