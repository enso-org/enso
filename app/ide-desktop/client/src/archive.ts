import { createReadStream } from 'node:fs'
import { readdir, stat } from 'node:fs/promises'
import { join, relative } from 'node:path'
import { Stream } from 'node:stream'
import { createGzip } from 'node:zlib'
import { pack as tarPack } from 'tar-stream'
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
        archive.entry(null, data, (error, entry) => (entry ? resolve() : reject(error)))
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
  const gzipStream = createGzip()
  const builder = tarWriteStream()
  builder.stream.pipe(gzipStream)
  return { ...builder, stream: gzipStream }
}

/** Add a folder and all its children recursively to an archive. */
export async function addFsFolderToArchive(
  builder: ArchiveBuilder,
  folderPath: string,
  data: ArchiveEntryMetadata,
) {
  const { name: rootPath } = data
  const onEntry = async (entryPath: string) => {
    const pathInFolder = relative(folderPath, entryPath)
    const pathInArchive = join(rootPath, pathInFolder)
    const entryStat = await stat(entryPath)
    if (!entryStat.isDirectory()) {
      await builder.addFile(createReadStream(pathInFolder), { name: pathInArchive })
    } else {
      await builder.addFolder({ name: pathInArchive })
      for (const entryName of await readdir(entryPath)) {
        const childPath = join(entryPath, entryName)
        await onEntry(childPath)
      }
    }
  }
  await onEntry(folderPath)
}
