import * as crypto from 'node:crypto'
import * as fsSync from 'node:fs'
import * as fs from 'node:fs/promises'
import * as path from 'node:path'
import type * as http from 'node:http'
import * as yaml from 'yaml'
import { Readable } from 'node:stream'
import {
  type Attributes,
  type DirectoryEntry,
  type FileEntry,
  type FileSystemEntry,
  FileSystemEntryType,
  type ProjectEntry,
  type ProjectMetadata,
} from './types.js'

export interface ProjectManagementModule {
  getMetadata(path: string): any | null
  PROJECT_METADATA_RELATIVE_PATH: string
}

/** JSON-RPC result wrapper */
export function toJSONRPCResult(result: unknown): string {
  return JSON.stringify({ jsonrpc: '2.0', id: 0, result })
}

/** JSON-RPC error wrapper */
export function toJSONRPCError(message: string, data?: unknown): string {
  return JSON.stringify({
    jsonrpc: '2.0',
    id: 0,
    error: { code: 0, message, ...(data != null ? { data } : {}) },
  })
}

/**
 * Return a {@link ProjectMetadata} if the metadata is a valid metadata object,
 * else return `null`.
 */
export function extractProjectMetadata(yamlObj: unknown, jsonObj: unknown): ProjectMetadata | null {
  if (
    typeof yamlObj !== 'object' ||
    yamlObj == null ||
    typeof jsonObj !== 'object' ||
    jsonObj == null
  ) {
    return null
  } else {
    const validDateString = (string: string) => {
      const date = new Date(string)
      return !Number.isNaN(Number(date)) ? date.toString() : null
    }
    const name = 'name' in yamlObj && typeof yamlObj.name === 'string' ? yamlObj.name : null
    const namespace =
      'namespace' in yamlObj && typeof yamlObj.namespace === 'string' ? yamlObj.namespace : 'local'
    const engineVersion =
      'edition' in yamlObj && typeof yamlObj.edition === 'string' ? yamlObj.edition : null
    const id = 'id' in jsonObj && typeof jsonObj.id === 'string' ? jsonObj.id : null
    const created =
      'created' in jsonObj && typeof jsonObj.created === 'string' ?
        validDateString(jsonObj.created)
      : null
    const lastOpened =
      'lastOpened' in jsonObj && typeof jsonObj.lastOpened === 'string' ?
        validDateString(jsonObj.lastOpened)
      : null
    if (name != null && id != null && created != null) {
      return {
        name,
        namespace,
        id,
        ...(engineVersion != null ? { engineVersion } : {}),
        created,
        ...(lastOpened != null ? { lastOpened } : {}),
      } satisfies ProjectMetadata
    } else {
      return null
    }
  }
}

/**
 * Checks if files that start with the dot.
 * Note on Windows does not check the hidden property.
 */
export function isHidden(filePath: string): boolean {
  const dotfile = /(^|[\\/])\.[^\\/]+$/g
  return dotfile.test(filePath)
}

/** Return whether a file exists. */
export async function fileExists(path: string): Promise<boolean> {
  try {
    await fs.stat(path)
    return true
  } catch {
    return false
  }
}

/** Handle filesystem commands */
export async function handleFilesystemCommand(
  cliArguments: string[],
  request?: http.IncomingMessage,
  projectManagement?: ProjectManagementModule,
): Promise<string | fsSync.ReadStream> {
  let result: string | fsSync.ReadStream = toJSONRPCError(
    `Error running Project Manager command.`,
    {
      command: cliArguments,
    },
  )

  try {
    switch (cliArguments[0]) {
      case '--filesystem-exists': {
        const directoryPath = cliArguments[1]
        if (directoryPath != null) {
          const exists = await fileExists(directoryPath)
          result = toJSONRPCResult({ exists })
        }
        break
      }
      case '--filesystem-list': {
        const directoryPath = cliArguments[1]
        if (directoryPath != null && projectManagement != null) {
          const entryNames = await fs.readdir(directoryPath)
          const entries: FileSystemEntry[] = []
          for (const entryName of entryNames) {
            const entryPath = path.join(directoryPath, entryName)
            if (isHidden(entryPath)) continue
            const stat = await fs.stat(entryPath)
            const attributes: Attributes = {
              byteSize: stat.size,
              creationTime: new Date(stat.ctimeMs).toISOString(),
              lastAccessTime: new Date(stat.atimeMs).toISOString(),
              lastModifiedTime: new Date(stat.mtimeMs).toISOString(),
            }
            if (stat.isFile()) {
              entries.push({
                type: FileSystemEntryType.FileEntry,
                path: entryPath,
                attributes,
              } satisfies FileEntry)
            } else {
              try {
                const packageMetadataPath = path.join(entryPath, 'package.yaml')
                const projectMetadataPath = path.join(
                  entryPath,
                  projectManagement.PROJECT_METADATA_RELATIVE_PATH,
                )
                const packageMetadataContents = await fs.readFile(packageMetadataPath)
                const packageMetadataYaml = yaml.parse(packageMetadataContents.toString())
                let projectMetadataJson
                try {
                  const projectMetadataContents = await fs.readFile(projectMetadataPath)
                  projectMetadataJson = JSON.parse(projectMetadataContents.toString())
                } catch (e) {
                  if (
                    'name' in packageMetadataYaml &&
                    typeof packageMetadataYaml.name === 'string'
                  ) {
                    projectMetadataJson = {
                      id: crypto.randomUUID(),
                      kind: 'UserProject',
                      created: new Date().toISOString(),
                      lastOpened: null,
                    }
                    await fs.mkdir(path.dirname(projectMetadataPath), { recursive: true })
                    await fs.writeFile(
                      projectMetadataPath,
                      JSON.stringify(projectMetadataJson),
                    )
                  } else {
                    throw e
                  }
                }
                const metadata = extractProjectMetadata(
                  packageMetadataYaml,
                  projectMetadataJson,
                )
                if (metadata != null) {
                  // This is a project.
                  entries.push({
                    type: FileSystemEntryType.ProjectEntry,
                    path: entryPath,
                    attributes,
                    metadata,
                  } satisfies ProjectEntry)
                } else {
                  // This error moves control flow to the
                  // `catch` clause directly below.
                  throw new Error('Invalid project metadata.')
                }
              } catch {
                // This is a regular directory, not a project.
                entries.push({
                  type: FileSystemEntryType.DirectoryEntry,
                  path: entryPath,
                  attributes,
                } satisfies DirectoryEntry)
              }
            }
          }
          result = toJSONRPCResult({ entries })
        }
        break
      }
      case '--filesystem-create-directory': {
        const directoryPath = cliArguments[1]
        if (directoryPath != null) {
          await fs.mkdir(directoryPath, { recursive: true })
          result = toJSONRPCResult(null)
        }
        break
      }
      case '--filesystem-read-path': {
        const filePath = cliArguments[1]
        if (filePath != null) {
          result = fsSync.createReadStream(filePath)
        }
        break
      }
      case '--filesystem-write-path': {
        const filePath = cliArguments[1]
        if (filePath != null && request != null) {
          await new Promise((resolve, reject) => {
            request
              .pipe(fsSync.createWriteStream(filePath), {
                end: true,
              })
              .on('close', resolve)
              .on('error', reject)
          })
          result = toJSONRPCResult(null)
        }
        break
      }
      case '--filesystem-move-from': {
        const sourcePath = cliArguments[1]
        const destinationPath = cliArguments[3]
        if (
          sourcePath != null &&
          cliArguments[2] === '--filesystem-move-to' &&
          destinationPath != null
        ) {
          await fs.rename(sourcePath, destinationPath)
          result = toJSONRPCResult(null)
        }
        break
      }
      case '--filesystem-delete': {
        const fileOrDirectoryPath = cliArguments[1]
        if (fileOrDirectoryPath != null) {
          await fs.rm(fileOrDirectoryPath, { recursive: true })
          result = toJSONRPCResult(null)
        }
        break
      }
      default: {
        const message = `Error in Project Manager shim: unknown command ${JSON.stringify(cliArguments)}`
        console.error(message)
        result = toJSONRPCError(message)
        break
      }
    }
  } catch {
    // Ignored. `result` retains its original value indicating an error.
  }

  return result
}

/**
 * Handle filesystem commands with simplified interface for the Electron server.
 * Returns a readable stream instead of string or ReadStream.
 */
export async function handleFilesystemCommandSimple(
  cliArguments: string[],
  request?: http.IncomingMessage,
  projectManagement?: { getMetadata(path: string): any | null },
): Promise<NodeJS.ReadableStream> {
  try {
    switch (cliArguments[0]) {
      case '--filesystem-exists': {
        const directoryPath = cliArguments[1]
        if (directoryPath != null) {
          const exists = await fileExists(directoryPath)
          const result = toJSONRPCResult({ exists })
          const readableStream = new Readable()
          readableStream.push(result)
          readableStream.push(null)
          return readableStream
        }
        break
      }
      case '--filesystem-list': {
        const directoryPath = cliArguments[1]
        if (directoryPath != null) {
          const entryNames = await fs.readdir(directoryPath)
          const entries: Array<{
            type: string
            path: string
            attributes: {
              byteSize: number
              creationTime: string
              lastAccessTime: string
              lastModifiedTime: string
            }
            metadata?: any
          }> = []

          for (const entryName of entryNames) {
            const entryPath = path.join(directoryPath, entryName)
            const stats = await fs.stat(entryPath)
            const attributes = {
              byteSize: stats.size,
              creationTime: new Date(stats.ctimeMs).toISOString(),
              lastAccessTime: new Date(stats.atimeMs).toISOString(),
              lastModifiedTime: new Date(stats.mtimeMs).toISOString(),
            }

            if (stats.isFile()) {
              entries.push({
                type: 'FileEntry',
                path: entryPath,
                attributes,
              })
            } else if (stats.isDirectory()) {
              // Check if it's a project
              const metadata = projectManagement?.getMetadata(entryPath)
              if (metadata) {
                entries.push({
                  type: 'ProjectEntry',
                  path: entryPath,
                  attributes,
                  metadata,
                })
              } else {
                entries.push({
                  type: 'DirectoryEntry',
                  path: entryPath,
                  attributes,
                })
              }
            }
          }

          const result = toJSONRPCResult({ entries })
          const readableStream = new Readable()
          readableStream.push(result)
          readableStream.push(null)
          return readableStream
        }
        break
      }
      case '--filesystem-create-directory': {
        const directoryPath = cliArguments[1]
        if (directoryPath != null) {
          await fs.mkdir(directoryPath, { recursive: true })
          const result = toJSONRPCResult(null)
          const readableStream = new Readable()
          readableStream.push(result)
          readableStream.push(null)
          return readableStream
        }
        break
      }
      case '--filesystem-read-path': {
        const filePath = cliArguments[1]
        if (filePath != null) {
          return fsSync.createReadStream(filePath)
        }
        break
      }
      case '--filesystem-write-path': {
        const filePath = cliArguments[1]
        if (filePath != null && request != null) {
          await new Promise((resolve, reject) => {
            request
              .pipe(fsSync.createWriteStream(filePath), { end: true })
              .on('close', resolve)
              .on('error', reject)
          })
          const result = toJSONRPCResult(null)
          const readableStream = new Readable()
          readableStream.push(result)
          readableStream.push(null)
          return readableStream
        }
        break
      }
      case '--filesystem-move-from': {
        const sourcePath = cliArguments[1]
        const destinationPath = cliArguments[3]
        if (
          sourcePath != null &&
          cliArguments[2] === '--filesystem-move-to' &&
          destinationPath != null
        ) {
          await fs.rename(sourcePath, destinationPath)
          const result = toJSONRPCResult(null)
          const readableStream = new Readable()
          readableStream.push(result)
          readableStream.push(null)
          return readableStream
        }
        break
      }
      case '--filesystem-delete': {
        const fileOrDirectoryPath = cliArguments[1]
        if (fileOrDirectoryPath != null) {
          await fs.rm(fileOrDirectoryPath, { recursive: true })
          const result = toJSONRPCResult(null)
          const readableStream = new Readable()
          readableStream.push(result)
          readableStream.push(null)
          return readableStream
        }
        break
      }
    }
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : String(error)
    const result = toJSONRPCError(`Filesystem operation failed: ${errorMessage}`)
    const readableStream = new Readable()
    readableStream.push(result)
    readableStream.push(null)
    return readableStream
  }

  // Default error case
  const readableStream = new Readable()
  readableStream.push(toJSONRPCError('Invalid command'))
  readableStream.push(null)
  return readableStream
}