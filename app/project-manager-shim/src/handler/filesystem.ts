import * as crypto from 'node:crypto'
import * as fsSync from 'node:fs'
import * as fs from 'node:fs/promises'
import type * as http from 'node:http'
import * as path from 'node:path'
import { promisify } from 'node:util'
import * as zlib from 'node:zlib'

import { Rfc3339DateTime } from 'enso-common/src/utilities/data/dateTime'
import * as yaml from 'yaml'
import { getEngineLogDirectory } from '../distributionManager.js'
import * as projectManagement from '../projectManagement.js'
import { resolveClashingProjectIds } from '../projectService/resolveClashingProjectIds.js'
import { toJSONRPCError, toJSONRPCResult } from './jsonrpc.js'

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
        const isRecursive = cliArguments[0] === '--filesystem-list-recursive'
        if (directoryPath == null) break
        const directoryPathQueue = [directoryPath]
        const entries: FileSystemEntry[] = []
        while (true) {
          const currentDirectoryPath = directoryPathQueue.shift()
          if (currentDirectoryPath == null) break
          const entryNames = await fs.readdir(currentDirectoryPath)
          const fileEntries = await Promise.all(
            entryNames.map(async (entryName) => {
              const entryPath = path.join(currentDirectoryPath, entryName)
              if (isFileHidden(entryPath)) return []
              return [await getFileSystemEntry(entryPath)]
            }),
          )
          const resolvedEntries = await resolveClashingProjectEntryIds(fileEntries.flat())
          entries.push(...resolvedEntries)
          for (const entry of resolvedEntries) {
            if (isRecursive && entry.type === FileSystemEntryType.DirectoryEntry) {
              directoryPathQueue.push(entry.path)
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
        const localProjectKey = cliArguments[1]
        if (localProjectKey == null) break
        result = toJSONRPCResult(await listProjectSessions(localProjectKey))
        break
      }
      case '--get-project-session-logs': {
        const sessionId = cliArguments[1]
        const scrollId = cliArguments[2] ?? null
        if (sessionId == null) break
        result = toJSONRPCResult(getProjectSessionLogs(sessionId, scrollId))
        break
      }
      case '--download-project-session-logs': {
        const sessionId = cliArguments[1]
        if (sessionId == null) break
        result = toJSONRPCResult(await downloadProjectSessionLogs(sessionId))
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

/** Update metadata ids of projects to avoid duplicates. */
async function resolveClashingProjectEntryIds(entries: readonly FileSystemEntry[]) {
  const rewrittenProjects = new Map<string, ProjectEntry>()
  const projects = entries.flatMap((entry) =>
    entry.type === FileSystemEntryType.ProjectEntry ?
      [
        {
          entry,
          id: entry.metadata.id,
          directoryCreationTime: entry.attributes.creationTime,
        },
      ]
    : [],
  )
  const resolvedProjects = await resolveClashingProjectIds(projects, async (project, newId) => {
    const projectMetadataPath = path.join(
      project.entry.path,
      projectManagement.PROJECT_METADATA_RELATIVE_PATH,
    )
    const projectMetadata = JSON.parse(await fs.readFile(projectMetadataPath, 'utf-8'))
    projectMetadata.id = newId
    await fs.writeFile(projectMetadataPath, JSON.stringify(projectMetadata, null, 2))
    return {
      ...project,
      id: newId,
      entry: {
        ...project.entry,
        metadata: {
          ...project.entry.metadata,
          id: newId,
        },
      },
    }
  })
  for (const project of resolvedProjects) {
    rewrittenProjects.set(project.entry.path, project.entry)
  }
  return entries.map((entry) =>
    entry.type === FileSystemEntryType.ProjectEntry ?
      (rewrittenProjects.get(entry.path) ?? entry)
    : entry,
  )
}

/** Get a file system entry for a given path. */
export async function getFileSystemEntry(entryPath: string): Promise<FileSystemEntry> {
  const stat = await fs.stat(entryPath)
  const attributes: Attributes = {
    byteSize: stat.size,
    creationTime: new Date(stat.birthtime).toISOString(),
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

// ============
// === Logs ===
// ============

const SESSION_ID_PREFIX = 'localprojectsession-'
const gunzipAsync = promisify(zlib.gunzip)

/** Validate that a path segment contains no directory traversal or separators. */
function isSafePathSegment(segment: string): boolean {
  return segment.length > 0 && segment !== '.' && segment !== '..' && !/[/\\]/.test(segment)
}

/**
 * Parse date-time from a log filename like `enso-language-server-2026-03-31-14-23-45`.
 * Expects the date-time at the end of the base name.
 */
function parseDateTimeFromFilename(baseName: string): Rfc3339DateTime | null {
  const match = baseName.match(/(\d{4})-(\d{2})-(\d{2})-(\d{2})-(\d{2})-(\d{2})$/)
  if (!match) return null
  return Rfc3339DateTime(`${match[1]}-${match[2]}-${match[3]}T${match[4]}:${match[5]}:${match[6]}`)
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

/**
 * Encode a session ID from local project key and file base name.
 * Format: `localprojectsession-{localProjectKey}/{baseName}`
 */
export function encodeSessionId(localProjectKey: string, baseName: string): string {
  return `${SESSION_ID_PREFIX}${localProjectKey}/${baseName}`
}

/**
 * Decode a session ID into local project key and file base name.
 * Returns the project log directory and the file base name.
 * Throws if the session ID contains path traversal attempts.
 */
function decodeSessionId(sessionId: string): { projectLogDir: string; baseName: string } {
  const raw =
    sessionId.startsWith(SESSION_ID_PREFIX) ? sessionId.slice(SESSION_ID_PREFIX.length) : sessionId
  const slashIdx = raw.indexOf('/')
  const logDir = getEngineLogDirectory()
  if (slashIdx < 0) {
    if (!isSafePathSegment(raw)) {
      throw new Error(`Invalid session ID: unsafe segment '${raw}'`)
    }
    return { projectLogDir: logDir, baseName: raw }
  }
  const localProjectKey = raw.slice(0, slashIdx)
  const baseName = raw.slice(slashIdx + 1)
  if (!isSafePathSegment(localProjectKey) || !isSafePathSegment(baseName)) {
    throw new Error(`Invalid session ID: unsafe path segments in '${sessionId}'`)
  }
  return { projectLogDir: path.join(logDir, localProjectKey), baseName }
}

/**
 * List project sessions by scanning `{logDir}/{localProjectKey}/` for log files.
 * Each unique base name (active log + its rolled archives) is one session.
 */
export async function listProjectSessions(
  localProjectKey: string,
): Promise<{ sessions: readonly { projectSessionId: string; createdAt: Rfc3339DateTime }[] }> {
  if (!isSafePathSegment(localProjectKey)) {
    throw new Error(`Invalid project key: unsafe segment '${localProjectKey}'`)
  }
  const projectLogDir = path.join(getEngineLogDirectory(), localProjectKey)
  let entries: string[]
  try {
    entries = await fs.readdir(projectLogDir)
  } catch (e: any) {
    // Suppress errors if the directory does not exist.
    if (e?.code !== 'ENOENT') {
      console.error(`Failed to read log directory '${projectLogDir}':`, e)
    }
    return { sessions: [] }
  }
  const seen = new Set<string>()
  const sessions: { projectSessionId: string; createdAt: Rfc3339DateTime }[] = []
  for (const entry of entries) {
    const baseName = extractSessionBaseName(entry)
    if (baseName == null) continue
    if (seen.has(baseName)) continue
    seen.add(baseName)
    const createdAt = parseDateTimeFromFilename(baseName)
    if (!createdAt) continue
    sessions.push({
      projectSessionId: encodeSessionId(localProjectKey, baseName),
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
 * - `null`        load archive 0 (or active log if no archives)
 * - `"archive:N"` load archive N of (N.log.gz rolled archives)
 * - `"active"`    load the active .log file
 * - `"done"`      return empty (signals end of pagination)
 */
export function getProjectSessionLogs(
  sessionId: string,
  scrollId: string | null,
): { scrollId: string; hits: readonly string[] } {
  if (scrollId === 'done') {
    return { scrollId: 'done', hits: [] }
  }
  const { projectLogDir, baseName } = decodeSessionId(sessionId)

  const sortedArchiveIndices = collectArchiveIndices(projectLogDir, baseName)
  const hasArchives = sortedArchiveIndices.length > 0

  if (scrollId == null) {
    if (hasArchives) {
      return readArchive(projectLogDir, baseName, sortedArchiveIndices, 0)
    }
    return readActiveLog(projectLogDir, baseName)
  }

  const archiveMatch = scrollId.match(/^archive:(\d+)$/)
  if (archiveMatch) {
    const requestedIndex = parseInt(archiveMatch[1]!, 10)
    const pos = sortedArchiveIndices.indexOf(requestedIndex)
    if (pos >= 0) {
      return readArchive(projectLogDir, baseName, sortedArchiveIndices, pos)
    }
    return readActiveLog(projectLogDir, baseName)
  }

  if (scrollId === 'active') {
    return readActiveLog(projectLogDir, baseName)
  }

  return { scrollId: 'done', hits: [] }
}

/** Collect sorted archive indices for a session base name. */
function collectArchiveIndices(dir: string, baseName: string): number[] {
  const prefix = `${baseName}.`
  const suffix = '.log.gz'
  const indices: number[] = []
  try {
    for (const entry of fsSync.readdirSync(dir)) {
      if (!entry.startsWith(prefix) || !entry.endsWith(suffix)) continue
      const middle = entry.slice(prefix.length, -suffix.length)
      if (/^\d+$/.test(middle)) {
        indices.push(parseInt(middle, 10))
      }
    }
  } catch (e) {
    console.error(`Failed to scan log directory '${dir}' for archives:`, e)
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
  dir: string,
  baseName: string,
  sortedIndices: number[],
  pos: number,
): { scrollId: string; hits: readonly string[] } {
  const archiveIndex = sortedIndices[pos]!
  const filePath = path.join(dir, `${baseName}.${archiveIndex}.log.gz`)
  let lines: string[] = []
  try {
    const compressed = fsSync.readFileSync(filePath)
    lines = splitLines(zlib.gunzipSync(compressed).toString('utf-8'))
  } catch (e) {
    console.error(`Failed to read archive '${filePath}':`, e)
  }
  const nextPos = pos + 1
  const nextScrollId =
    nextPos < sortedIndices.length ? `archive:${sortedIndices[nextPos]}` : 'active'
  return { scrollId: nextScrollId, hits: lines }
}

/** Read the active log file. */
function readActiveLog(
  dir: string,
  baseName: string,
): { scrollId: string; hits: readonly string[] } {
  const logPath = path.join(dir, `${baseName}.log`)
  try {
    const content = fsSync.readFileSync(logPath, 'utf-8')
    return { scrollId: 'done', hits: splitLines(content) }
  } catch (e) {
    console.error(`Failed to read log file '${logPath}':`, e)
    return { scrollId: 'done', hits: [] }
  }
}

/** Read all log files for a session and return them concatenated as a single string. */
export async function downloadProjectSessionLogs(sessionId: string): Promise<string> {
  const { projectLogDir, baseName } = decodeSessionId(sessionId)
  const sortedArchiveIndices = collectArchiveIndices(projectLogDir, baseName)
  const parts: string[] = []

  for (const archiveIndex of sortedArchiveIndices) {
    const filePath = path.join(projectLogDir, `${baseName}.${archiveIndex}.log.gz`)
    try {
      const compressed = await fs.readFile(filePath)
      parts.push((await gunzipAsync(compressed)).toString('utf-8'))
    } catch (e) {
      console.error(`Failed to read archive '${filePath}':`, e)
    }
  }

  const activeLogPath = path.join(projectLogDir, `${baseName}.log`)
  try {
    parts.push(await fs.readFile(activeLogPath, 'utf-8'))
  } catch (e) {
    console.error(`Failed to read log file '${activeLogPath}':`, e)
  }

  return parts.join('')
}
