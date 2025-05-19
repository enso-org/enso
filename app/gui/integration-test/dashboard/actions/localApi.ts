import * as backend from '#/services/Backend'
import {
  ProjectName,
  type Attributes,
  type DirectoryEntry,
  type FileEntry,
  type FileSystemEntry,
  type JSONRPCResponse,
  type OpenProject,
  type OpenProjectParams,
  type Path,
  type ProjectEntry,
  type ProjectMetadata,
  type ProjectState,
  type UUID,
} from '#/services/ProjectManager/types'
import { toRfc3339 } from 'enso-common/src/utilities/data/dateTime'
import type { Page } from 'playwright'

function array<T>(): Readonly<T>[] {
  return []
}

const ROOT_PATH = '/home/me'

const INITIAL_CALLS_OBJECT = {
  getFileContent: array<{ path: string }>(),
  openProject: array<OpenProjectParams>(),
}

const READONLY_INITIAL_CALLS_OBJECT: TrackedCallsInternal = INITIAL_CALLS_OBJECT

export { READONLY_INITIAL_CALLS_OBJECT as INITIAL_CALLS_OBJECT }

type TrackedCallsInternal = {
  [K in keyof typeof INITIAL_CALLS_OBJECT]: Readonly<(typeof INITIAL_CALLS_OBJECT)[K]>
}

interface JSONRPCRequest<Method extends string, Params> {
  jsonrpc: '2.0'
  id: number
  method: Method
  params: Params
}

type ProjectManagerJsonRpcRequest = JSONRPCRequest<'project/open', OpenProjectParams>

type FileSystemEntryWithData =
  | { type: 'DirectoryEntry'; entry: DirectoryEntry; children: FileSystemEntryWithData[] }
  | {
      type: 'ProjectEntry'
      entry: ProjectEntry
      id: UUID
      metadata: {
        projectName: ProjectName
        projectNormalizedName: string
      }
    }
  | { type: 'FileEntry'; entry: FileEntry; content: string }

/**
 * Setup function for the mock API.
 * use it to setup the mock API with custom handlers.
 */
export interface SetupLocalAPI {
  (api: Awaited<ReturnType<typeof localMockApi>>): Promise<void> | void
}

/** Parameters for {@link mockApi}. */
export interface MockParams {
  readonly page: Page
  readonly setupLocalAPI?: SetupLocalAPI | null | undefined
}
/** The return type of {@link localMockApi}. */
export interface MockApi extends Awaited<ReturnType<typeof localMockApiInternal>> {}

export const localMockApi: (params: MockParams) => Promise<MockApi> = localMockApiInternal

/** Add route handlers for the mock API to a page. */
async function localMockApiInternal({ page, setupLocalAPI }: MockParams) {
  const fileSystem = new Map<string, FileSystemEntryWithData>()
  const openProjects = new Map<UUID, ProjectState>()
  const projectParentPaths = new Map<UUID, string>()

  const callsObjects = new Set<typeof INITIAL_CALLS_OBJECT>()

  function trackCalls() {
    const calls = structuredClone(INITIAL_CALLS_OBJECT)
    callsObjects.add(calls)
    return calls
  }

  function pushToKey<Object extends Record<keyof Object, unknown[]>, Key extends keyof Object>(
    object: Object,
    key: Key,
    item: Object[Key][number],
  ) {
    object[key].push(item)
  }

  function called<Key extends keyof typeof INITIAL_CALLS_OBJECT>(
    key: Key,
    args: (typeof INITIAL_CALLS_OBJECT)[Key][number],
  ) {
    for (const callsObject of callsObjects) {
      pushToKey(callsObject, key, args)
    }
  }

  const createAttributes = (attributes: Partial<Attributes> | undefined = {}): Attributes => ({
    creationTime: toRfc3339(new Date()),
    lastAccessTime: toRfc3339(new Date()),
    lastModifiedTime: toRfc3339(new Date()),
    byteSize: 0,
    ...attributes,
  })

  const createDirectory = ({
    path,
    attributes,
  }: {
    path: Path
    attributes?: Partial<Attributes>
  }): DirectoryEntry => ({
    type: 'DirectoryEntry',
    path,
    attributes: createAttributes(attributes),
  })

  const createProject = ({
    path,
    metadata,
    attributes,
  }: {
    path: Path
    metadata: ProjectMetadata
    attributes?: Partial<Attributes>
  }): ProjectEntry => ({
    type: 'ProjectEntry',
    path,
    metadata,
    attributes: createAttributes(attributes),
  })

  const createFile = ({
    path,
    attributes,
  }: {
    path: Path
    attributes?: Partial<Attributes>
  }): FileEntry => ({
    type: 'FileEntry',
    path,
    attributes: createAttributes(attributes),
  })

  await page.routeWebSocket('ws://127.0.0.1:30535/', (ws) => {
    ws.onMessage(async (messageRaw) => {
      const message: ProjectManagerJsonRpcRequest = JSON.parse(messageRaw.toString('utf-8'))

      let delay = 0
      let response: JSONRPCResponse<unknown>
      const toJSONRPCResult = (result: unknown): JSONRPCResponse<unknown> => ({
        jsonrpc: '2.0',
        id: message.id,
        result,
      })
      const toJSONRPCError = (errorMessage: string): JSONRPCResponse<unknown> => ({
        jsonrpc: '2.0',
        id: message.id,
        error: { code: 0, message: errorMessage },
      })

      switch (message.method) {
        case 'project/open': {
          const params = message.params
          called('openProject', params)
          const parentDirectory = fileSystem.get(
            projectParentPaths.get(params.projectId) ?? ROOT_PATH,
          )
          const project =
            parentDirectory?.type === 'DirectoryEntry' ?
              parentDirectory.children.find(
                (entry) => entry.type === 'ProjectEntry' && entry.id === params.projectId,
              )
            : null
          if (project?.type !== 'ProjectEntry') {
            response = toJSONRPCError(`No project with UUID '${params.projectId}'`)
            break
          }
          const result: OpenProject = {
            engineVersion: '0.0.0-dev',
            languageServerBinaryAddress: { host: 'ws://localhost', port: 1234 },
            languageServerJsonAddress: { host: 'ws://localhost', port: 1235 },
            projectNamespace: 'local',
            ...project.metadata,
          }
          openProjects.set(params.projectId, {
            state: backend.ProjectState.opened,
            data: result,
          })
          delay = 1_000
          response = toJSONRPCResult(result)
          break
        }
        default: {
          response = toJSONRPCError(`Unknown Project Manager JSON-RPC method '${message.method}'`)
          break
        }
      }

      await new Promise((resolve) => {
        setTimeout(resolve, delay)
      })

      ws.send(JSON.stringify(response))
    })
  })

  await page.route(
    'https://localhost:8080/api/run-project-manager-command',
    async (route, request) => {
      const cliArgumentsRaw = JSON.stringify(
        new URL(request.url()).searchParams.get('cli-arguments'),
      )
      const cliArgumentsObject =
        cliArgumentsRaw[0] != null ?
          { name: cliArgumentsRaw[0].slice(2), arguments: cliArgumentsRaw.slice(1) }
        : null
      if (!cliArgumentsObject) {
        return route.fulfill({ status: 400 })
      }
      const cliArguments = cliArgumentsObject.arguments
      switch (cliArgumentsObject.name) {
        case 'filesystem-list': {
          const folderPath = cliArguments[0]
          const folder = folderPath != null ? fileSystem.get(folderPath) : null
          if (folder?.type !== 'DirectoryEntry') {
            return route.fulfill({ status: 400 })
          }
          const entries: readonly FileSystemEntry[] = folder.children.map(({ entry }) => entry)
          return route.fulfill({
            contentType: 'application/json',
            body: JSON.stringify({ entries }),
          })
          break
        }
        case 'filesystem-read-path': {
          const filePath = cliArguments[0]
          if (filePath == null) {
            return route.fulfill({ status: 400 })
          }
          called('getFileContent', { path: filePath })
          const file = fileSystem.get(filePath)
          if (file?.type !== 'FileEntry') {
            return route.fulfill({ status: 400 })
          }
          return route.fulfill({ contentType: 'text/plain', body: file.content })
        }
      }
    },
  )

  const api = { trackCalls, createDirectory, createProject, createFile } as const

  await setupLocalAPI?.(api)

  return api
}
