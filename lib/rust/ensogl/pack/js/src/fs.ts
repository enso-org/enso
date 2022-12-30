export * from 'fs/promises'
import { ObjectEncodingOptions, OpenMode, PathLike } from 'node:fs'
import { FileHandle } from 'fs/promises'
import { Abortable } from 'node:events'
import { promises as fs } from 'fs'
import { Task as LoggedTask } from './task'

export async function readFile(
    path: PathLike | FileHandle,
    options?:
        | ({
              encoding?: null | undefined
              flag?: OpenMode | undefined
          } & Abortable)
        | null
): Promise<Buffer>

export async function readFile(
    path: PathLike | FileHandle,
    options:
        | ({
              encoding: BufferEncoding
              flag?: OpenMode | undefined
          } & Abortable)
        | BufferEncoding
): Promise<string>

export async function readFile(
    path: PathLike | FileHandle,
    options?:
        | (ObjectEncodingOptions &
              Abortable & {
                  flag?: OpenMode | undefined
              })
        | BufferEncoding
        | null
): Promise<string | Buffer> {
    return LoggedTask.asyncWith(`Reading file '${path}'.`, async () => {
        return await fs.readFile(path, options)
    })
}
