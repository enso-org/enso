import * as crypto from 'node:crypto'
import * as fsSync from 'node:fs'
import * as fs from 'node:fs/promises'
import type * as http from 'node:http'
import * as path from 'node:path'
import * as zlib from 'node:zlib'
import * as yaml from 'yaml'
import * as projectManagement from '../projectManagement.js'
import { toJSONRPCError, toJSONRPCResult } from './jsonrpc.js'
import { getEngineLogDirectory } from '../distributionManager.js'

// =======================
// === ProjectMetadata ===
// =======================

/** Details of a project. */
export interface ProjectMetadata {
  /** The name of the project. */
  readonly name: string
  /** The namespace of the project. */
  readonly namespace: string
  /** The project id. */
  readonly id: string
  /**
   * The Enso Engine version to use for the project, represented by a semver version
   * string.
   *
   * If the edition associated with the project could not be resolved, the
   * engine version may be missing.
   */
  readonly engineVersion?: string
  /** The project creation time. */
  readonly created: string
  /** The last opened datetime. */
  readonly lastOpened?: string
}

// ==================
// === Attributes ===
// ==================

/** Attributes of a file or folder. */
export interface Attributes {
  readonly creationTime: string
  readonly lastAccessTime: string
  readonly lastModifiedTime: string
  readonly byteSize: number
}

// =======================
// === FileSystemEntry ===
// =======================

/** Metadata for an arbitrary file system entry. */
export type FileSystemEntry = DirectoryEntry | FileEntry | ProjectEntry

/** The discriminator value for {@link FileSystemEntry}. */
export enum FileSystemEntryType {
  DirectoryEntry = 'DirectoryEntry',
  ProjectEntry = 'ProjectEntry',
  FileEntry = 'FileEntry',
}

/** Metadata for a file. */
export interface FileEntry {
  readonly type: FileSystemEntryType.FileEntry
  readonly path: string
  readonly attributes: Attributes
}

/** Metadata for a directory. */
export interface DirectoryEntry {
  readonly type: FileSystemEntryType.DirectoryEntry
  readonly path: string
  readonly attributes: Attributes
}

/** Metadata for a project. */
export interface ProjectEntry {
  readonly type: FileSystemEntryType.ProjectEntry
  readonly path: string
  readonly metadata: ProjectMetadata
  readonly attributes: Attributes
}

// ================
// === Handlers ===
// ================

/**
 * Return a {@link ProjectMetadata} if the metadata is a valid metadata object,
 * else return `null`.
 */
function extractProjectMetadata(yamlObj: unknown, jsonObj: unknown): ProjectMetadata | null {
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
function isFileHidden(filePath: string): boolean {
  const dotfile = /(^|[\\/])\.[^\\/]+$/g
  return dotfile.test(filePath)
}

/** Return whether a file exists. */
async function fileExists(path: string): Promise<boolean> {
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
  request: http.IncomingMessage,
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
      case '--filesystem-list':
      case '--filesystem-list-recursive': {
        const directoryPath = cliArguments[1]
        const isRecursive = cliArguments[1] === '--filesystem-list-recursive'
        if (directoryPath == null) break
        const directoryPathQueue = [directoryPath]
        const entries: FileSystemEntry[] = []
        while (true) {
          const currentDirectoryPath = directoryPathQueue.shift()
          if (currentDirectoryPath == null) break
          const entryNames = await fs.readdir(currentDirectoryPath)
          for (const entryName of entryNames) {
            const entryPath = path.join(currentDirectoryPath, entryName)
            if (isFileHidden(entryPath)) continue
            const entry = await getFileSystemEntry(entryPath)
            entries.push(entry)
            if (isRecursive && entry.type === FileSystemEntryType.DirectoryEntry) {
              directoryPathQueue.push(entryPath)
            }
          }
        }
        result = toJSONRPCResult({ entries })
        break
      }
      case '--filesystem-create-directory': {
        const directoryPath = cliArguments[1]
        if (directoryPath == null) break
        await fs.mkdir(directoryPath, { recursive: true })
        result = toJSONRPCResult(null)
        break
      }
      case '--filesystem-read-path': {
        const filePath = cliArguments[1]
        if (filePath == null) break
        result = fsSync.createReadStream(filePath)
        break
      }
      case '--filesystem-write-path': {
        const filePath = cliArguments[1]
        if (filePath == null) break
        await new Promise<void>((resolve, reject) => {
          request
            .pipe(fsSync.createWriteStream(filePath), {
              end: true,
            })
            .on('close', resolve)
            .on('error', reject)
        })
        result = toJSONRPCResult(null)
        break
      }
      case '--filesystem-move-from': {
        const sourcePath = cliArguments[1]
        const destinationPath = cliArguments[3]
        if (
          sourcePath == null ||
          cliArguments[2] !== '--filesystem-move-to' ||
          destinationPath == null
        )
          break
        await fs.rename(sourcePath, destinationPath)
        result = toJSONRPCResult(null)
        break
      }
      case '--filesystem-delete': {
        const fileOrDirectoryPath = cliArguments[1]
        if (fileOrDirectoryPath == null) break
        await fs.rm(fileOrDirectoryPath, { recursive: true })
        result = toJSONRPCResult(null)
        break
      }
      case '--list-project-sessions': {
        const projectId = cliArguments[1]
        if (projectId == null) break
        result = toJSONRPCResult(await listProjectSessions(projectId))
        break
      }
      case '--get-project-session-logs': {
        const sessionId = cliArguments[1]
        const scrollId = cliArguments[2] ?? null
        if (sessionId == null) break
        result = toJSONRPCResult(getProjectSessionLogs(sessionId, scrollId))
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

/** Get a file system entry for a given path. */
export async function getFileSystemEntry(entryPath: string): Promise<FileSystemEntry> {
  const stat = await fs.stat(entryPath)
  const attributes: Attributes = {
    byteSize: stat.size,
    creationTime: new Date(stat.ctimeMs).toISOString(),
    lastAccessTime: new Date(stat.atimeMs).toISOString(),
    lastModifiedTime: new Date(stat.mtimeMs).toISOString(),
  }
  if (stat.isFile()) {
    return {
      type: FileSystemEntryType.FileEntry,
      path: entryPath,
      attributes,
    }
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
        if ('name' in packageMetadataYaml && typeof packageMetadataYaml.name === 'string') {
          projectMetadataJson = {
            id: crypto.randomUUID(),
            kind: 'UserProject',
            created: new Date().toISOString(),
            lastOpened: null,
          }
          await fs.mkdir(path.dirname(projectMetadataPath), { recursive: true })
          await fs.writeFile(projectMetadataPath, JSON.stringify(projectMetadataJson))
        } else {
          throw e
        }
      }
      const metadata = extractProjectMetadata(packageMetadataYaml, projectMetadataJson)
      if (metadata != null) {
        // This is a project.
        return {
          type: FileSystemEntryType.ProjectEntry,
          path: entryPath,
          attributes,
          metadata,
        }
      } else {
        // This error moves control flow to the
        // `catch` clause directly below.
        throw new Error('Invalid project metadata.')
      }
    } catch {
      // This is a regular directory, not a project.
      return {
        type: FileSystemEntryType.DirectoryEntry,
        path: entryPath,
        attributes,
      }
    }
  }
}

const SESSION_ID_PREFIX = 'localprojectsession-'

/** Parse date-time from a log filename segment like `2026-03-31-14-23-45`. */
function parseDateTimeFromFilename(filename: string, projectId: string): string | null {
  const idx = filename.indexOf(projectId)
  if (idx < 0) return null
  const after = filename.slice(idx + projectId.length + 1)
  const match = after.match(/^(\d{4})-(\d{2})-(\d{2})-(\d{2})-(\d{2})-(\d{2})/)
  if (!match) return null
  return `${match[1]}-${match[2]}-${match[3]}T${match[4]}:${match[5]}:${match[6]}`
}

/**
 * Extract the session base name from a log filename.
 * Matches active logs (`base.log`) and rolled archives (`base.N.log.gz`).
 * Returns the base name (without `.log` / `.N.log.gz`) or null if not a log file.
 */
function extractSessionBaseName(filename: string): string | null {
  const archiveMatch = filename.match(/^(.+)\.\d+\.log\.gz$/)
  if (archiveMatch) return archiveMatch[1]!
  if (filename.endsWith('.log')) return filename.slice(0, -4)
  return null
}

/** List project sessions by scanning the engine log directory for matching log files. */
async function listProjectSessions(
  projectId: string,
): Promise<{ sessions: readonly { projectSessionId: string; createdAt: string }[] }> {
  const logDir = getEngineLogDirectory()
  let entries: string[]
  try {
    entries = await fs.readdir(logDir)
  } catch {
    return { sessions: [] }
  }
  const seen = new Set<string>()
  const sessions: { projectSessionId: string; createdAt: string }[] = []
  for (const entry of entries) {
    const baseName = extractSessionBaseName(entry)
    if (baseName == null) continue
    if (!baseName.includes(`-${projectId}-`)) continue
    if (seen.has(baseName)) continue
    seen.add(baseName)
    const createdAt = parseDateTimeFromFilename(baseName, projectId)
    if (!createdAt) continue
    sessions.push({
      projectSessionId: `${SESSION_ID_PREFIX}${baseName}`,
      createdAt,
    })
  }
  sessions.sort((a, b) => a.createdAt.localeCompare(b.createdAt))
  return { sessions }
}

/**
 * Read log file content for a given local project session.
 *
 * Uses scrollId to load archives one-by-one:
 * - `null`        → load archive 0 (or active log if no archives)
 * - `"archive:N"` → load archive N
 * - `"active"`    → load the active .log file
 * - `"done"`      → return empty (signals end of pagination)
 */
function getProjectSessionLogs(
  sessionId: string,
  scrollId: string | null,
): { scrollId: string; hits: readonly string[] } {
  if (scrollId === 'done') {
    return { scrollId: 'done', hits: [] }
  }
  const baseName = sessionId.startsWith(SESSION_ID_PREFIX)
    ? sessionId.slice(SESSION_ID_PREFIX.length)
    : sessionId
  const logDir = getEngineLogDirectory()

  const sortedArchiveIndices = collectArchiveIndices(logDir, baseName)
  const hasArchives = sortedArchiveIndices.length > 0

  if (scrollId == null) {
    // First request: if archives exist, start with the first one; otherwise load active log
    if (hasArchives) {
      return readArchive(logDir, baseName, sortedArchiveIndices, 0)
    }
    return readActiveLog(logDir, baseName)
  }

  const archiveMatch = scrollId.match(/^archive:(\d+)$/)
  if (archiveMatch) {
    const requestedIndex = parseInt(archiveMatch[1]!, 10)
    const pos = sortedArchiveIndices.indexOf(requestedIndex)
    if (pos >= 0) {
      return readArchive(logDir, baseName, sortedArchiveIndices, pos)
    }
    // Requested archive not found, fall through to active log
    return readActiveLog(logDir, baseName)
  }

  if (scrollId === 'active') {
    return readActiveLog(logDir, baseName)
  }

  return { scrollId: 'done', hits: [] }
}

/** Collect sorted archive indices for a session base name. */
function collectArchiveIndices(logDir: string, baseName: string): number[] {
  const indices: number[] = []
  try {
    for (const entry of fsSync.readdirSync(logDir)) {
      const match = entry.match(new RegExp(`^${escapeRegExp(baseName)}\\.(\\d+)\\.log\\.gz$`))
      if (match) {
        indices.push(parseInt(match[1]!, 10))
      }
    }
  } catch {
    // log directory may not exist
  }
  indices.sort((a, b) => a - b)
  return indices
}

/** Split text into lines, dropping a trailing empty line caused by a final newline. */
function splitLines(text: string): string[] {
  const lines = text.split('\n')
  if (lines.length > 0 && lines[lines.length - 1] === '') {
    lines.pop()
  }
  return lines
}

/** Read a single rolled archive and return the scrollId pointing to the next chunk. */
function readArchive(
  logDir: string,
  baseName: string,
  sortedIndices: number[],
  pos: number,
): { scrollId: string; hits: readonly string[] } {
  const archiveIndex = sortedIndices[pos]!
  const filePath = path.join(logDir, `${baseName}.${archiveIndex}.log.gz`)
  let lines: string[] = []
  try {
    const compressed = fsSync.readFileSync(filePath)
    lines = splitLines(zlib.gunzipSync(compressed).toString('utf-8'))
  } catch {
    // skip unreadable archive
  }
  const nextPos = pos + 1
  const nextScrollId =
    nextPos < sortedIndices.length ? `archive:${sortedIndices[nextPos]}` : 'active'
  return { scrollId: nextScrollId, hits: lines }
}

/** Read the active log file. */
function readActiveLog(
  logDir: string,
  baseName: string,
): { scrollId: string; hits: readonly string[] } {
  const logPath = path.join(logDir, `${baseName}.log`)
  try {
    const content = fsSync.readFileSync(logPath, 'utf-8')
    return { scrollId: 'done', hits: splitLines(content) }
  } catch {
    return { scrollId: 'done', hits: [] }
  }
}

function escapeRegExp(s: string): string {
  return s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')
}
