import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { ProjectState as BackendProjectState } from '../../Backend'
import { ProjectManager } from '../ProjectManager'
import {
  MissingComponentAction,
  Path,
  UTCDateTime,
  UUID,
  type FileSystemEntry,
  type OpenProjectParams,
} from '../types'

const ROOT = Path('/root')
const PROJECT_ID = '11111111-1111-1111-1111-111111111111'
const FIRST_PARENT = '/root/cloud-first'
const SECOND_PARENT = '/root/cloud-second'
const FIRST_PROJECT_PATH = Path(`${FIRST_PARENT}/project_root`)
const SECOND_PROJECT_PATH = Path(`${SECOND_PARENT}/project_root`)

function jsonRpcResult<T>(result: T) {
  return {
    jsonrpc: '2.0' as const,
    id: 1,
    result,
  }
}

function projectEntry(path: Path): FileSystemEntry {
  const mockCreationTime = UTCDateTime('2026-01-01T00:00:00.000Z')
  return {
    type: 'ProjectEntry',
    path,
    attributes: {
      creationTime: mockCreationTime,
      lastAccessTime: mockCreationTime,
      lastModifiedTime: mockCreationTime,
      byteSize: 0,
    },
    metadata: {
      name: 'Project',
      namespace: 'local',
      id: UUID(PROJECT_ID),
      created: mockCreationTime,
    },
  }
}

describe('ProjectManager', () => {
  const openCalls: OpenProjectParams[] = []
  const closeCalls: { projectId: string; projectsDirectory: string }[] = []

  beforeEach(() => {
    openCalls.length = 0
    closeCalls.length = 0

    vi.stubGlobal(
      'fetch',
      vi.fn(async (input: string | URL | Request, init?: RequestInit) => {
        const url =
          typeof input === 'string' ? input
          : input instanceof URL ? input.toString()
          : input.url
        if (url.startsWith('/api/run-project-manager-command?')) {
          const parsed = new URL(url, 'https://example.test')
          const cliArguments = JSON.parse(
            parsed.searchParams.get('cli-arguments') ?? '[]',
          ) as string[]
          const command = cliArguments[0]
          const targetPath = cliArguments[1]?.replace(/\/$/, '')
          if (command !== '--filesystem-list') {
            throw new Error(`Unexpected standalone command: ${String(command)}`)
          }
          const entries =
            targetPath === FIRST_PARENT ? [projectEntry(FIRST_PROJECT_PATH)]
            : targetPath === SECOND_PARENT ? [projectEntry(SECOND_PROJECT_PATH)]
            : []
          return {
            json: async () => jsonRpcResult({ entries }),
          } as Response
        }

        if (url === '/api/project-service/project/open') {
          const body = JSON.parse(String(init?.body)) as OpenProjectParams
          openCalls.push(body)
          const port = body.projectsDirectory === Path(FIRST_PARENT) ? 4101 : 4102
          return {
            json: async () =>
              jsonRpcResult({
                projectId: body.projectId,
                languageServerJsonAddress: { host: '127.0.0.1', port },
                languageServerYdocAddress: { host: '127.0.0.1', port: port + 1000 },
                projectName: body.projectsDirectory === Path(FIRST_PARENT) ? 'First' : 'Second',
                projectNormalizedName:
                  body.projectsDirectory === Path(FIRST_PARENT) ? 'First' : 'Second',
                projectNamespace: 'local',
              }),
          } as Response
        }

        if (url === '/api/project-service/project/close') {
          const body = JSON.parse(String(init?.body)) as {
            projectId: string
            projectsDirectory: string
          }
          closeCalls.push(body)
          return {
            json: async () => jsonRpcResult(null),
          } as Response
        }

        throw new Error(`Unexpected fetch URL: ${url}`)
      }),
    )
  })

  afterEach(() => {
    vi.unstubAllGlobals()
  })

  it('opens projects with the same embedded id independently when their paths differ', async () => {
    const projectManager = new ProjectManager(ROOT)

    const first = await projectManager.openProject({
      projectPath: FIRST_PROJECT_PATH,
      missingComponentAction: MissingComponentAction.install,
    })
    const second = await projectManager.openProject({
      projectPath: SECOND_PROJECT_PATH,
      missingComponentAction: MissingComponentAction.install,
    })

    expect(first.languageServerJsonAddress.port).toBe(4101)
    expect(second.languageServerJsonAddress.port).toBe(4102)
    expect(openCalls).toEqual([
      {
        projectId: PROJECT_ID,
        projectsDirectory: FIRST_PARENT,
        missingComponentAction: MissingComponentAction.install,
      },
      {
        projectId: PROJECT_ID,
        projectsDirectory: SECOND_PARENT,
        missingComponentAction: MissingComponentAction.install,
      },
    ])

    const firstState = await projectManager.getProject(FIRST_PROJECT_PATH)
    const secondState = await projectManager.getProject(SECOND_PROJECT_PATH)

    expect(firstState?.state).toBe(BackendProjectState.opened)
    expect(secondState?.state).toBe(BackendProjectState.opened)
    expect((await firstState?.data)?.languageServerJsonAddress.port).toBe(4101)
    expect((await secondState?.data)?.languageServerJsonAddress.port).toBe(4102)
  })

  it('closing one path does not clear another open project with the same embedded id', async () => {
    const projectManager = new ProjectManager(ROOT)

    await projectManager.openProject({
      projectPath: FIRST_PROJECT_PATH,
      missingComponentAction: MissingComponentAction.install,
    })
    await projectManager.openProject({
      projectPath: SECOND_PROJECT_PATH,
      missingComponentAction: MissingComponentAction.install,
    })

    await projectManager.closeProject({ projectPath: SECOND_PROJECT_PATH })

    const firstProject = await projectManager.getProject(FIRST_PROJECT_PATH)
    expect(firstProject?.state).toBe(BackendProjectState.opened)
    expect(closeCalls).toEqual([
      {
        projectId: PROJECT_ID,
        projectsDirectory: SECOND_PARENT,
      },
    ])
  })

  it('derives different project telemtry keys for same embedded id in different parent directories', async () => {
    const projectManager = new ProjectManager(ROOT)

    const firstKey = await projectManager.getTelemetryKey(FIRST_PROJECT_PATH)
    const secondKey = await projectManager.getTelemetryKey(SECOND_PROJECT_PATH)

    expect(firstKey).toMatch(/^local-[0-9a-f]{64}$/)
    expect(secondKey).toMatch(/^local-[0-9a-f]{64}$/)
    expect(firstKey).not.toBe(secondKey)
  })
})
