/** @file This module redefines some `node:fs` functions with embedded logging, so it is easy to
 * track what they do. */

import {
    MakeDirectoryOptions,
    Mode,
    ObjectEncodingOptions,
    OpenMode,
    PathLike,
    RmDirOptions,
    RmOptions,
} from 'node:fs'
import { FileHandle } from 'fs/promises'
import { Abortable } from 'node:events'
import { promises as fs } from 'fs'
import * as log from 'runner/log'
import { logger } from 'runner/log/logger'
import { Stream } from 'node:stream'

// ================
// === readFile ===
// ================

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

/** Read a file and log the operation. */
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
    return log.Task.asyncWith(`Reading file '${String(path)}'.`, async () => {
        return await fs.readFile(path, options)
    })
}

// ==============
// === unlink ===
// ==============

/** Unlink a file and log the operation. */
export async function unlink(path: PathLike): Promise<void> {
    logger.log(`Removing file '${String(path)}'.`)
    return await fs.unlink(path)
}

// =============
// === rmdir ===
// =============

/** Remove a directory and log the operation. */
export async function rmdir(path: PathLike, options?: RmDirOptions): Promise<void> {
    logger.log(`Removing directory '${String(path)}'.`)
    return await fs.rmdir(path, options)
}

// =============
// === mkdir ===
// =============

/** Make a directory and log the operation. */
export async function mkdir(
    path: PathLike,
    options?: Mode | MakeDirectoryOptions | null
): Promise<string | undefined> {
    logger.log(`Creating directory '${String(path)}'.`)
    return await fs.mkdir(path, options)
}

// ==========
// === rm ===
// ==========

/** Remove a file or directory and log the operation. */
export async function rm(path: PathLike, options?: RmOptions): Promise<void> {
    logger.log(`Removing '${String(path)}'.`)
    return await fs.rm(path, options)
}

// =================
// === writeFile ===
// =================

/** Write a file and log the operation. */
export async function writeFile(
    file: PathLike | FileHandle,
    data:
        | string
        | NodeJS.ArrayBufferView
        | Iterable<string | NodeJS.ArrayBufferView>
        | AsyncIterable<string | NodeJS.ArrayBufferView>
        | Stream,
    options?:
        | (ObjectEncodingOptions & {
              mode?: Mode | undefined
              flag?: OpenMode | undefined
          } & Abortable)
        | BufferEncoding
        | null
): Promise<void> {
    logger.log(`Writing file '${String(file)}'.`)
    return await fs.writeFile(file, data, options)
}
