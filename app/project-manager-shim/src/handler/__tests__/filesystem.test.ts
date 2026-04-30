import * as fs from 'node:fs'
import * as fsPromises from 'node:fs/promises'
import * as os from 'node:os'
import * as path from 'node:path'
import * as zlib from 'node:zlib'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import {
  downloadProjectSessionLogs,
  encodeSessionId,
  getProjectSessionLogs,
  handleFilesystemCommand,
  listProjectSessions,
} from '../filesystem'

async function createProject(directory: string, id: string, name = path.basename(directory)) {
  await fsPromises.mkdir(path.join(directory, '.enso'), { recursive: true })
  await fsPromises.writeFile(
    path.join(directory, 'package.yaml'),
    `name: ${name}\nnamespace: local\n`,
  )
  await fsPromises.writeFile(
    path.join(directory, '.enso/project.json'),
    JSON.stringify({ id, kind: 'UserProject', created: '2026-01-01T00:00:00.000Z' }, null, 2),
  )
}

async function listEntries(directory: string, recursive = false) {
  const response = await handleFilesystemCommand(
    [recursive ? '--filesystem-list-recursive' : '--filesystem-list', directory],
    {} as any,
  )
  if (typeof response !== 'string') {
    throw new Error('Expected JSON-RPC string response.')
  }
  return (JSON.parse(response) as { result: { entries: unknown[] } }).result.entries
}

describe('filesystem listing duplicate resolution', () => {
  let tmpDir: string

  beforeEach(async () => {
    tmpDir = await fsPromises.mkdtemp(path.join(os.tmpdir(), 'filesystem-list-test-'))
  })

  afterEach(async () => {
    await fsPromises.rm(tmpDir, { recursive: true, force: true, maxRetries: 5, retryDelay: 1000 })
  })

  test('dedupes sibling project IDs during listing and rewrites younger metadata on disk', async () => {
    const duplicateId = '11111111-1111-1111-1111-111111111111'
    const olderProject = path.join(tmpDir, 'older')
    const youngerProject = path.join(tmpDir, 'younger')

    await createProject(olderProject, duplicateId, 'Older')
    await new Promise((resolve) => setTimeout(resolve, 20))
    await createProject(youngerProject, duplicateId, 'Younger')

    const entries = (await listEntries(tmpDir)).filter(
      (
        entry,
      ): entry is {
        type: 'ProjectEntry'
        path: string
        metadata: { id: string; name: string }
      } =>
        typeof entry === 'object' &&
        entry != null &&
        'type' in entry &&
        entry.type === 'ProjectEntry' &&
        'metadata' in entry,
    )
    const olderEntry = entries.find((entry) => entry.path === olderProject)
    const youngerEntry = entries.find((entry) => entry.path === youngerProject)
    const youngerMetadata = JSON.parse(
      await fsPromises.readFile(path.join(youngerProject, '.enso/project.json'), 'utf-8'),
    )

    expect(entries).toHaveLength(2)
    expect(olderEntry?.metadata.id).toBe(duplicateId)
    expect(youngerEntry?.metadata.id).toMatch(
      /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/,
    )
    expect(youngerEntry?.metadata.id).not.toBe(duplicateId)
    expect(youngerMetadata.id).toBe(youngerEntry?.metadata.id)
  })

  test('recursive listing does not dedupe same IDs across different parent directories', async () => {
    const duplicateId = '22222222-2222-2222-2222-222222222222'
    const firstParent = path.join(tmpDir, 'first-parent')
    const secondParent = path.join(tmpDir, 'second-parent')
    const firstProject = path.join(firstParent, 'project')
    const secondProject = path.join(secondParent, 'project')

    await fsPromises.mkdir(firstParent, { recursive: true })
    await fsPromises.mkdir(secondParent, { recursive: true })
    await createProject(firstProject, duplicateId, 'First')
    await createProject(secondProject, duplicateId, 'Second')

    const projects = (await listEntries(tmpDir, true)).filter(
      (entry): entry is { type: 'ProjectEntry'; path: string; metadata: { id: string } } =>
        typeof entry === 'object' &&
        entry != null &&
        'type' in entry &&
        entry.type === 'ProjectEntry' &&
        'metadata' in entry,
    )

    expect(projects.find((entry) => entry.path === firstProject)?.metadata.id).toBe(duplicateId)
    expect(projects.find((entry) => entry.path === secondProject)?.metadata.id).toBe(duplicateId)
  })
})

describe('listProjectSessions', () => {
  let tmpDir: string
  let projectId: string

  beforeEach(async () => {
    tmpDir = await fsPromises.mkdtemp(path.join(os.tmpdir(), 'session-test-'))
    projectId = 'test-project-id'
    const projectLogDir = path.join(tmpDir, projectId)
    await fsPromises.mkdir(projectLogDir, { recursive: true })
    vi.stubEnv('ENSO_LOG_DIRECTORY', tmpDir)
  })

  afterEach(async () => {
    vi.unstubAllEnvs()
    try {
      await fsPromises.rm(tmpDir, { recursive: true, force: true, maxRetries: 5, retryDelay: 1000 })
    } catch (error) {
      console.error('Failed to clean up temp directory:', error)
    }
  })

  test('returns empty sessions for non-existent directory', async () => {
    const result = await listProjectSessions('nonexistent-project')
    expect(result.sessions).toEqual([])
  })

  test('returns empty sessions for directory with no log files', async () => {
    const projectLogDir = path.join(tmpDir, projectId)
    fs.writeFileSync(path.join(projectLogDir, 'readme.txt'), 'not a log')
    const result = await listProjectSessions(projectId)
    expect(result.sessions).toEqual([])
  })

  test('lists sessions from active log files', async () => {
    const projectLogDir = path.join(tmpDir, projectId)
    fs.writeFileSync(
      path.join(projectLogDir, 'enso-language-server-2026-03-31-14-23-45.log'),
      'log content',
    )
    fs.writeFileSync(
      path.join(projectLogDir, 'enso-language-server-2026-04-01-10-00-00.log'),
      'log content',
    )

    const result = await listProjectSessions(projectId)
    expect(result.sessions).toHaveLength(2)
    expect(result.sessions[0]!.createdAt).toBe('2026-03-31T14:23:45')
    expect(result.sessions[1]!.createdAt).toBe('2026-04-01T10:00:00')
  })

  test('deduplicates active log and its archives into one session', async () => {
    const projectLogDir = path.join(tmpDir, projectId)
    const baseName = 'enso-language-server-2026-03-31-14-23-45'
    fs.writeFileSync(path.join(projectLogDir, `${baseName}.log`), 'active log')
    fs.writeFileSync(path.join(projectLogDir, `${baseName}.0.log.gz`), zlib.gzipSync('archive 0'))
    fs.writeFileSync(path.join(projectLogDir, `${baseName}.1.log.gz`), zlib.gzipSync('archive 1'))

    const result = await listProjectSessions(projectId)
    expect(result.sessions).toHaveLength(1)
    expect(result.sessions[0]!.createdAt).toBe('2026-03-31T14:23:45')
  })

  test('sorts sessions oldest-first', async () => {
    const projectLogDir = path.join(tmpDir, projectId)
    // Write in reverse order to ensure sorting is not just insertion order
    fs.writeFileSync(
      path.join(projectLogDir, 'enso-language-server-2026-12-25-10-30-00.log'),
      'newer',
    )
    fs.writeFileSync(
      path.join(projectLogDir, 'enso-language-server-2026-01-01-00-00-00.log'),
      'older',
    )
    fs.writeFileSync(
      path.join(projectLogDir, 'enso-language-server-2026-06-15-12-00-00.log'),
      'middle',
    )

    const result = await listProjectSessions(projectId)
    expect(result.sessions).toHaveLength(3)
    expect(result.sessions.map((s) => s.createdAt)).toEqual([
      '2026-01-01T00:00:00',
      '2026-06-15T12:00:00',
      '2026-12-25T10:30:00',
    ])
  })

  test('ignores files without parseable date-time in name', async () => {
    const projectLogDir = path.join(tmpDir, projectId)
    fs.writeFileSync(path.join(projectLogDir, 'some-random-file.log'), 'no date')
    fs.writeFileSync(
      path.join(projectLogDir, 'enso-language-server-2026-03-31-14-23-45.log'),
      'valid',
    )

    const result = await listProjectSessions(projectId)
    expect(result.sessions).toHaveLength(1)
  })

  test('session IDs contain the project ID', async () => {
    const projectLogDir = path.join(tmpDir, projectId)
    fs.writeFileSync(
      path.join(projectLogDir, 'enso-language-server-2026-03-31-14-23-45.log'),
      'log',
    )

    const result = await listProjectSessions(projectId)
    expect(result.sessions[0]!.projectSessionId).toContain(projectId)
    expect(result.sessions[0]!.projectSessionId).toContain(
      'enso-language-server-2026-03-31-14-23-45',
    )
  })
})

describe('getProjectSessionLogs', () => {
  let tmpDir: string
  let projectId: string

  beforeEach(async () => {
    tmpDir = await fsPromises.mkdtemp(path.join(os.tmpdir(), 'logs-test-'))
    projectId = 'test-project-id'
    const projectLogDir = path.join(tmpDir, projectId)
    await fsPromises.mkdir(projectLogDir, { recursive: true })
    vi.stubEnv('ENSO_LOG_DIRECTORY', tmpDir)
  })

  afterEach(async () => {
    vi.unstubAllEnvs()
    await fsPromises.rm(tmpDir, { recursive: true, force: true, maxRetries: 5, retryDelay: 1000 })
  })

  test('returns done with empty hits when scrollId is "done"', () => {
    const sessionId = encodeSessionId(projectId, 'enso-language-server-2026-03-31-14-23-45')
    const result = getProjectSessionLogs(sessionId, 'done')
    expect(result).toEqual({ scrollId: 'done', hits: [] })
  })

  test('reads active log when no archives exist (scrollId=null)', () => {
    const baseName = 'enso-language-server-2026-03-31-14-23-45'
    const projectLogDir = path.join(tmpDir, projectId)
    fs.writeFileSync(path.join(projectLogDir, `${baseName}.log`), 'line1\nline2\nline3\n')

    const sessionId = encodeSessionId(projectId, baseName)
    const result = getProjectSessionLogs(sessionId, null)
    expect(result.scrollId).toBe('done')
    expect(result.hits).toEqual(['line1', 'line2', 'line3'])
  })

  test('reads first archive when archives exist (scrollId=null)', () => {
    const baseName = 'enso-language-server-2026-03-31-14-23-45'
    const projectLogDir = path.join(tmpDir, projectId)
    fs.writeFileSync(path.join(projectLogDir, `${baseName}.log`), 'active content\n')
    fs.writeFileSync(
      path.join(projectLogDir, `${baseName}.0.log.gz`),
      zlib.gzipSync('archive0-line1\narchive0-line2\n'),
    )

    const sessionId = encodeSessionId(projectId, baseName)
    const result = getProjectSessionLogs(sessionId, null)
    expect(result.hits).toEqual(['archive0-line1', 'archive0-line2'])
    // Next scrollId should point to active since there's only archive 0
    expect(result.scrollId).toBe('active')
  })

  test('paginates through archives then active log', () => {
    const baseName = 'enso-language-server-2026-03-31-14-23-45'
    const projectLogDir = path.join(tmpDir, projectId)
    fs.writeFileSync(path.join(projectLogDir, `${baseName}.log`), 'active\n')
    fs.writeFileSync(path.join(projectLogDir, `${baseName}.0.log.gz`), zlib.gzipSync('archive0\n'))
    fs.writeFileSync(path.join(projectLogDir, `${baseName}.1.log.gz`), zlib.gzipSync('archive1\n'))

    const sessionId = encodeSessionId(projectId, baseName)

    // First call: reads archive 0
    const page1 = getProjectSessionLogs(sessionId, null)
    expect(page1.hits).toEqual(['archive0'])
    expect(page1.scrollId).toBe('archive:1')

    // Second call: reads archive 1
    const page2 = getProjectSessionLogs(sessionId, page1.scrollId)
    expect(page2.hits).toEqual(['archive1'])
    expect(page2.scrollId).toBe('active')

    // Third call: reads active log
    const page3 = getProjectSessionLogs(sessionId, page2.scrollId)
    expect(page3.hits).toEqual(['active'])
    expect(page3.scrollId).toBe('done')

    // Fourth call: done
    const page4 = getProjectSessionLogs(sessionId, page3.scrollId)
    expect(page4.hits).toEqual([])
    expect(page4.scrollId).toBe('done')
  })

  test('returns empty hits when log file does not exist', () => {
    const baseName = 'enso-language-server-2026-03-31-14-23-45'
    const sessionId = encodeSessionId(projectId, baseName)
    const result = getProjectSessionLogs(sessionId, null)
    expect(result.scrollId).toBe('done')
    expect(result.hits).toEqual([])
  })

  test('handles non-sequential archive indices', () => {
    const baseName = 'enso-language-server-2026-03-31-14-23-45'
    const projectLogDir = path.join(tmpDir, projectId)
    fs.writeFileSync(path.join(projectLogDir, `${baseName}.log`), 'active\n')
    fs.writeFileSync(path.join(projectLogDir, `${baseName}.2.log.gz`), zlib.gzipSync('archive2\n'))
    fs.writeFileSync(path.join(projectLogDir, `${baseName}.5.log.gz`), zlib.gzipSync('archive5\n'))

    const sessionId = encodeSessionId(projectId, baseName)

    const page1 = getProjectSessionLogs(sessionId, null)
    expect(page1.hits).toEqual(['archive2'])
    expect(page1.scrollId).toBe('archive:5')

    const page2 = getProjectSessionLogs(sessionId, page1.scrollId)
    expect(page2.hits).toEqual(['archive5'])
    expect(page2.scrollId).toBe('active')
  })

  test('falls back to active log for unknown scrollId', () => {
    const baseName = 'enso-language-server-2026-03-31-14-23-45'
    const projectLogDir = path.join(tmpDir, projectId)
    fs.writeFileSync(path.join(projectLogDir, `${baseName}.log`), 'active\n')

    const sessionId = encodeSessionId(projectId, baseName)
    const result = getProjectSessionLogs(sessionId, 'unknown-scroll-id')
    expect(result.scrollId).toBe('done')
    expect(result.hits).toEqual([])
  })

  test('rejects session ID with .. in project ID', () => {
    expect(() => getProjectSessionLogs('localprojectsession-../../etc/base', null)).toThrow(
      /unsafe/i,
    )
  })

  test('rejects session ID with .. in base name', () => {
    expect(() => getProjectSessionLogs('localprojectsession-proj/../../../etc/base', null)).toThrow(
      /unsafe/i,
    )
  })

  test('rejects session ID with path separators', () => {
    expect(() => getProjectSessionLogs('localprojectsession-proj/sub/dir', null)).toThrow(/unsafe/i)
  })
})

describe('downloadProjectSessionLogs', () => {
  let tmpDir: string
  let projectId: string

  beforeEach(async () => {
    tmpDir = await fsPromises.mkdtemp(path.join(os.tmpdir(), 'download-test-'))
    projectId = 'test-project-id'
    const projectLogDir = path.join(tmpDir, projectId)
    await fsPromises.mkdir(projectLogDir, { recursive: true })
    vi.stubEnv('ENSO_LOG_DIRECTORY', tmpDir)
  })

  afterEach(async () => {
    vi.unstubAllEnvs()
    await fsPromises.rm(tmpDir, { recursive: true, force: true, maxRetries: 5, retryDelay: 1000 })
  })

  test('returns active log content when no archives exist', async () => {
    const baseName = 'enso-language-server-2026-03-31-14-23-45'
    const projectLogDir = path.join(tmpDir, projectId)
    fs.writeFileSync(path.join(projectLogDir, `${baseName}.log`), 'line1\nline2\n')

    const sessionId = encodeSessionId(projectId, baseName)
    await expect(downloadProjectSessionLogs(sessionId)).resolves.toBe('line1\nline2\n')
  })

  test('concatenates archives and active log in order', async () => {
    const baseName = 'enso-language-server-2026-03-31-14-23-45'
    const projectLogDir = path.join(tmpDir, projectId)
    fs.writeFileSync(path.join(projectLogDir, `${baseName}.0.log.gz`), zlib.gzipSync('archive0\n'))
    fs.writeFileSync(path.join(projectLogDir, `${baseName}.1.log.gz`), zlib.gzipSync('archive1\n'))
    fs.writeFileSync(path.join(projectLogDir, `${baseName}.log`), 'active\n')

    const sessionId = encodeSessionId(projectId, baseName)
    await expect(downloadProjectSessionLogs(sessionId)).resolves.toBe(
      'archive0\narchive1\nactive\n',
    )
  })

  test('handles non-sequential archive indices in sorted order', async () => {
    const baseName = 'enso-language-server-2026-03-31-14-23-45'
    const projectLogDir = path.join(tmpDir, projectId)
    fs.writeFileSync(path.join(projectLogDir, `${baseName}.5.log.gz`), zlib.gzipSync('five\n'))
    fs.writeFileSync(path.join(projectLogDir, `${baseName}.2.log.gz`), zlib.gzipSync('two\n'))
    fs.writeFileSync(path.join(projectLogDir, `${baseName}.log`), 'active\n')

    const sessionId = encodeSessionId(projectId, baseName)
    await expect(downloadProjectSessionLogs(sessionId)).resolves.toBe('two\nfive\nactive\n')
  })

  test('returns empty string when no log files exist', async () => {
    const baseName = 'enso-language-server-2026-03-31-14-23-45'
    const sessionId = encodeSessionId(projectId, baseName)
    await expect(downloadProjectSessionLogs(sessionId)).resolves.toBe('')
  })

  test('returns only archives when active log does not exist', async () => {
    const baseName = 'enso-language-server-2026-03-31-14-23-45'
    const projectLogDir = path.join(tmpDir, projectId)
    fs.writeFileSync(path.join(projectLogDir, `${baseName}.0.log.gz`), zlib.gzipSync('archive0\n'))

    const sessionId = encodeSessionId(projectId, baseName)
    await expect(downloadProjectSessionLogs(sessionId)).resolves.toBe('archive0\n')
  })

  test('returns only active log when archives do not exist', async () => {
    const baseName = 'enso-language-server-2026-03-31-14-23-45'
    const projectLogDir = path.join(tmpDir, projectId)
    fs.writeFileSync(path.join(projectLogDir, `${baseName}.log`), 'only active\n')

    const sessionId = encodeSessionId(projectId, baseName)
    await expect(downloadProjectSessionLogs(sessionId)).resolves.toBe('only active\n')
  })
})
