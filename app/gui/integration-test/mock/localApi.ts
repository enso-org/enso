import { capitalizeFirst } from '#/utilities/string'
import * as backend from 'enso-common/src/services/Backend'
import {
  Path,
  ProjectName,
  UUID,
  type Attributes,
  type CloseProjectParams,
  type CreateProject,
  type CreateProjectParams,
  type DirectoryEntry,
  type FileEntry,
  type FileSystemEntry,
  type JSONRPCResponse,
  type OpenProject,
  type OpenProjectParams,
  type ProjectEntry,
  type ProjectMetadata,
  type ProjectState,
} from 'enso-common/src/services/ProjectManager/types'
import { toRfc3339 } from 'enso-common/src/utilities/data/dateTime'
import { unsafeMutable } from 'enso-common/src/utilities/data/object'
import { getDirectoryAndName } from 'enso-common/src/utilities/file'
import { uniqueString } from 'enso-common/src/utilities/uniqueString'
import { test } from 'integration-test/base'
import { uuidv4 } from 'lib0/random.js'
import { join } from 'node:path'
import type { Page, WebSocketRoute } from 'playwright'
import { WSSharedDoc, YjsConnection, type YjsSocket } from 'ydoc-server'
import { makeVisUpdates, mockDataHandler, mockLSHandler, mockYdocProvider } from './lsHandler'

function array<T>(): Readonly<T>[] {
  return []
}

const ROOT_PARENT_PATH = Path('/home/user/enso')
const ROOT_PATH = Path('/home/user/enso/enso-projects')
const DOWNLOAD_PATH = Path('/home/user/enso/Downloads')

const languageServerJsonAddress = { host: '127.0.0.1', port: 1235 }
const languageServerBinaryAddress = { host: '127.0.0.1', port: 1234 }
const languageServerYdocAddress = { host: '127.0.0.1', port: 1233 }

const INITIAL_CALLS_OBJECT = {
  getRootDirectory: array<object>(),
  getDownloadDirectory: array<object>(),
  downloadCloudProject: array<object>(),
  downloadProject: array<{ projectId: backend.ProjectId }>(),
  getFileContent: array<{ path: string }>(),
  createProject: array<CreateProjectParams>(),
  openProject: array<OpenProjectParams>(),
  closeProject: array<CloseProjectParams>(),
}

const READONLY_INITIAL_CALLS_OBJECT: TrackedCallsInternal = INITIAL_CALLS_OBJECT

export { READONLY_INITIAL_CALLS_OBJECT as INITIAL_LOCAL_CALLS_OBJECT }

type TrackedCallsInternal = {
  [K in keyof typeof INITIAL_CALLS_OBJECT]: Readonly<(typeof INITIAL_CALLS_OBJECT)[K]>
}

export interface LocalTrackedCalls extends TrackedCallsInternal {}

type DirectoryEntryWithData = {
  type: 'DirectoryEntry'
  entry: DirectoryEntry
  children: FileSystemEntryWithData[]
}

type ProjectEntryWithData = {
  type: 'ProjectEntry'
  entry: ProjectEntry
  metadata: {
    projectName: ProjectName
    projectNormalizedName: string
  }
}

type FileEntryWithData = { type: 'FileEntry'; entry: FileEntry; content: string }

type FileSystemEntryWithData = DirectoryEntryWithData | ProjectEntryWithData | FileEntryWithData

/** The return type of {@link mockLocalApi}. */
export interface MockLocalApi extends Awaited<ReturnType<typeof mockLocalApi>> {}

/** Add route handlers for the mock API to a page. */
export async function mockLocalApi(page: Page) {
  const fileSystem = new Map<string, FileSystemEntryWithData>()
  const openProjects = new Map<UUID, ProjectState>()

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

  type DirectoryEntryOptions = {
    path: Path
    attributes?: Partial<Attributes>
  }

  const createDirectoryEntry = ({ path, attributes }: DirectoryEntryOptions): DirectoryEntry => ({
    type: 'DirectoryEntry',
    path,
    attributes: createAttributes(attributes),
  })

  const createDirectory = (options: DirectoryEntryOptions): DirectoryEntryWithData => {
    const entry = createDirectoryEntry(options)
    return { type: 'DirectoryEntry', entry, children: [] }
  }

  const addEntry = <E extends FileSystemEntryWithData>(path: Path, entry: E) => {
    fileSystem.set(path, entry)
    const { directoryPath } = getDirectoryAndName(path)
    const parentEntry = fileSystem.get(directoryPath)
    if (parentEntry?.type === 'DirectoryEntry') {
      parentEntry.children.push(entry)
    }
    return entry
  }

  const removeEntry = (path: Path) => {
    const entry = fileSystem.get(path)
    if (!entry) {
      return
    }
    fileSystem.delete(path)
    const { directoryPath } = getDirectoryAndName(path)
    const parentEntry = fileSystem.get(directoryPath)
    if (parentEntry?.type !== 'DirectoryEntry') {
      return
    }
    const index = parentEntry.children.findIndex((child) => child.entry.path === path)
    if (index !== -1) {
      parentEntry.children.splice(index, 1)
    }
    return entry
  }

  const moveEntry = (source: Path, destination: Path) => {
    if (!fileSystem.has(source) || fileSystem.has(destination)) {
      return
    }
    const entry = removeEntry(source)
    if (!entry) {
      return
    }
    unsafeMutable(entry.entry).path = destination
    addEntry(destination, entry)
    return entry
  }

  const addDirectory = (options: DirectoryEntryOptions) => {
    addEntry(options.path, createDirectory(options))
  }

  type ProjectEntryOptions = {
    path?: Path
    metadata: ProjectMetadata
    attributes?: Partial<Attributes>
  }

  const createProject = (options: ProjectEntryOptions): ProjectEntryWithData => {
    const normalizedName = options.metadata.name
      .split(' ')
      .filter((n) => n.length)
      .map((part) => capitalizeFirst(part))
      .join('_')
    return {
      type: 'ProjectEntry',
      entry: {
        type: 'ProjectEntry',
        path: options.path ?? Path(`${ROOT_PATH}/${normalizedName}`),
        metadata: options.metadata,
        attributes: createAttributes(options.attributes),
      },
      metadata: {
        projectName: ProjectName(options.metadata.name),
        projectNormalizedName: normalizedName,
      },
    }
  }

  const addProject = (options: ProjectEntryOptions) => {
    const project = createProject(options)
    return addEntry(project.entry.path, project)
  }

  type FileEntryOptions = {
    path: Path
    attributes?: Partial<Attributes>
    content?: string
  }

  const createFileEntry = ({ path, attributes }: FileEntryOptions): FileEntry => ({
    type: 'FileEntry',
    path,
    attributes: createAttributes(attributes),
  })

  const createFile = (options: FileEntryOptions): FileEntryWithData => {
    const { content = '' } = options
    const entry = createFileEntry(options)
    return { type: 'FileEntry', entry, content }
  }

  const addFile = (options: FileEntryOptions) => {
    addEntry(options.path, createFile(options))
  }

  addDirectory({ path: ROOT_PARENT_PATH })
  addDirectory({ path: ROOT_PATH })
  addDirectory({ path: DOWNLOAD_PATH })

  let languageServerBinaryWs: WebSocketRoute | null = null

  await test.step('Mock Local API', async () => {
    const toJSONRPCResult = (result: unknown): JSONRPCResponse<unknown> => ({
      jsonrpc: '2.0',
      id: 0,
      result,
    })
    const toJSONRPCError = (errorMessage: string): JSONRPCResponse<unknown> => ({
      jsonrpc: '2.0',
      id: 0,
      error: { code: 0, message: errorMessage },
    })

    await page.route('/api/project-service/project/create', async (route, request) => {
      if (request.method() !== 'POST') {
        return route.fulfill({ status: 400 })
      }
      const params: CreateProjectParams = JSON.parse(request.postData() ?? '{}')
      called('createProject', params)
      const parentPath = params.projectsDirectory ?? ROOT_PATH
      const path = Path(`${parentPath}/${params.name}`)
      const id = UUID(uuidv4())
      const metadata: ProjectEntryWithData['metadata'] = {
        projectName: params.name,
        projectNormalizedName: params.name,
      }
      const result: CreateProject = { projectId: id, projectPath: path, ...metadata }
      addProject({
        path,
        metadata: {
          id,
          name: metadata.projectName,
          namespace: 'local',
          created: toRfc3339(new Date()),
        },
      })
      return route.fulfill({
        contentType: 'application/json',
        body: JSON.stringify(toJSONRPCResult(result)),
      })
    })

    await page.route('/api/project-service/project/open', async (route, request) => {
      if (request.method() !== 'POST') {
        return route.fulfill({ status: 400 })
      }
      const params: OpenProjectParams = JSON.parse(request.postData() ?? '{}')
      called('openProject', params)
      const parentDirectory = fileSystem.get(params.projectsDirectory ?? ROOT_PATH)
      const project =
        parentDirectory?.type === 'DirectoryEntry' ?
          parentDirectory.children.find(
            (entry) =>
              entry.type === 'ProjectEntry' && entry.entry.metadata.id === params.projectId,
          )
        : null
      if (project?.type !== 'ProjectEntry') {
        return route.fulfill({
          contentType: 'application/json',
          body: JSON.stringify(toJSONRPCError(`No project with UUID '${params.projectId}'`)),
        })
      }
      unsafeMutable(project.entry.metadata).lastOpened = toRfc3339(new Date())
      const result: OpenProject = {
        languageServerBinaryAddress,
        languageServerJsonAddress,
        languageServerYdocAddress,
        projectNamespace: 'local',
        projectName: project.metadata.projectName,
        projectNormalizedName: project.metadata.projectNormalizedName,
      }
      openProjects.set(params.projectId, {
        state: backend.ProjectState.opened,
        data: result,
      })
      await new Promise((resolve) => {
        setTimeout(resolve, 1_000)
      })
      return route.fulfill({
        contentType: 'application/json',
        body: JSON.stringify(toJSONRPCResult(result)),
      })
    })

    await page.route('/api/project-service/project/close', async (route, request) => {
      if (request.method() !== 'POST') {
        return route.fulfill({ status: 400 })
      }
      const params: CloseProjectParams = JSON.parse(request.postData() ?? '{}')
      called('closeProject', params)
      openProjects.delete(params.projectId)
      return route.fulfill({
        contentType: 'application/json',
        body: JSON.stringify(toJSONRPCResult(null)),
      })
    })

    await page.routeWebSocket(
      `ws://${languageServerBinaryAddress.host}:${languageServerBinaryAddress.port}/`,
      (ws) => {
        languageServerBinaryWs = ws
        ws.onMessage(async (messageRaw) => {
          const response = await mockDataHandler(new Uint8Array(Buffer.from(messageRaw)).buffer)
          if (response) {
            ws.send(Buffer.from(response))
          }
        })
      },
    )
    await page.routeWebSocket(
      `ws://${languageServerJsonAddress.host}:${languageServerJsonAddress.port}/`,
      (ws) => {
        ws.onMessage(async (messageRaw) => {
          const { method, params, jsonrpc, id } = JSON.parse(messageRaw.toString())
          try {
            const result =
              (await mockLSHandler(
                method,
                params,
                (message) => ws.send(JSON.stringify({ jsonrpc, ...message })),
                (binaryData?: ArrayBuffer) => {
                  if (binaryData) languageServerBinaryWs?.send(Buffer.from(binaryData))
                },
              )) ?? null
            ws.send(JSON.stringify({ jsonrpc, id, result }))
          } catch (error) {
            ws.send(JSON.stringify({ jsonrpc, id, error }))
          }
        })
      },
    )
    const ydocAddressBase = `ws://${languageServerYdocAddress.host}:${languageServerYdocAddress.port}`

    class MockWs implements YjsSocket {
      binaryType = 'arraybuffer' as const
      readyState = WebSocket.OPEN
      constructor(private wsRoute: WebSocketRoute) {}
      on(event: 'close', listener: (code: number, reason: Buffer) => void): this
      on(event: 'message', listener: (data: ArrayBuffer | Buffer, isBinary: boolean) => void): this
      on(event: 'ping' | 'pong', listener: (data: Buffer) => void): this
      on(event: unknown, listener: unknown): this {
        switch (event) {
          case 'close':
            this.wsRoute.onClose((code, reason) => {
              const _listener = listener as (code: number, reason: Buffer) => void
              _listener(code ?? 0, Buffer.from(reason ?? []))
            })
            return this
          case 'message':
            this.wsRoute.onMessage((data) => {
              const _listener = listener as (data: ArrayBuffer | Buffer, isBinary: boolean) => void
              _listener(Buffer.from(data), true)
            })
            return this
          case 'ping':
          case 'pong':
            return this
        }
        throw new Error(`Event ${event} not implemented.`)
      }
      send(data: Uint8Array, cb?: (err?: Error) => void): void {
        this.wsRoute.send(Buffer.from(data))
        if (cb) Promise.resolve().then(() => cb())
      }
      ping(): void {}
      close(): void {
        this.wsRoute.close()
      }
    }

    await page.routeWebSocket(`${ydocAddressBase}/**`, (wsRoute) => {
      const parsedUrl = new URL(wsRoute.url())
      const room = parsedUrl.pathname.substring('/project/'.length)

      const mockWs = new MockWs(wsRoute)
      const wsDoc = new WSSharedDoc()
      const _connection = new YjsConnection(mockWs, wsDoc)
      mockYdocProvider(room, wsDoc.doc)
    })

    await page.route('/api/root-directory-path', async (route, request) => {
      called('getRootDirectory', {})
      if (request.method() !== 'GET') {
        return route.fulfill({ status: 400 })
      }
      return route.fulfill({
        contentType: 'text/plain',
        body: ROOT_PATH,
      })
    })

    await page.route('/api/download-directory-path', async (route, request) => {
      called('getDownloadDirectory', {})
      if (request.method() !== 'GET') {
        return route.fulfill({ status: 400 })
      }
      return route.fulfill({
        contentType: 'text/plain',
        body: DOWNLOAD_PATH,
      })
    })

    await page.route('/api/cloud/download-project?*', async (route, request) => {
      called('downloadCloudProject', {})
      if (request.method() !== 'GET') {
        return route.fulfill({ status: 400 })
      }
      const projectId = uniqueString()
      const parentDirectory = join(ROOT_PATH, `cloud-project-${projectId}`)
      const targetDirectory = join(parentDirectory, 'project_root')
      addDirectory({ path: Path(parentDirectory) })
      addProject({
        path: Path(targetDirectory),
        metadata: {
          name: '',
          namespace: '',
          created: toRfc3339(new Date()),
          id: UUID(uuidv4()),
        },
      })

      await route.fulfill({
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          targetDirectory,
          parentDirectory,
        }),
      })
    })

    await page.route('/api/upload-file?*', async (route, request) => {
      const params = new URL(request.url()).searchParams
      const fileName = params.get('file_name')
      const directoryPathRaw = params.get('directory') as backend.DirectoryId | null
      const directoryPath =
        directoryPathRaw != null ? backend.extractTypeAndPath(directoryPathRaw).path : ROOT_PATH
      const filePath = Path(`${directoryPath}/${fileName}`)
      if (filePath == null) {
        return route.fulfill({ status: 400 })
      }
      // Remove any existing file, if one exists.
      removeEntry(filePath)
      addFile({ path: filePath, content: request.postData() ?? '' })
      return route.fulfill({ body: filePath, contentType: 'text/plain' })
    })

    await page.route('/api/projects/**', async (route, request) => {
      const url = new URL(request.url())
      const { projectId: projectIdRaw } =
        url.pathname.match(/^\/api\/projects\/(?<projectId>[^/]+)\/download$/)?.groups ?? {}
      if (request.method() !== 'GET' || projectIdRaw == null) {
        return route.fulfill({ status: 400 })
      }
      const projectId = backend.ProjectId(projectIdRaw)
      called('downloadProject', { projectId })
      const response = `mock project body projectId='${projectId}'`
      return route.fulfill({
        contentType: 'text/plain',
        body: JSON.stringify(response),
      })
    })

    await page.route('/api/run-project-manager-command?*', async (route, request) => {
      const toJSONRPCResult = (result: unknown): JSONRPCResponse<unknown> => ({
        jsonrpc: '2.0',
        id: 0,
        result,
      })
      const succeed = (result: unknown, contentType = 'application/json') => {
        return route.fulfill({ contentType, body: JSON.stringify(toJSONRPCResult(result)) })
      }
      const toJSONRPCError = (errorMessage: string): JSONRPCResponse<unknown> => ({
        jsonrpc: '2.0',
        id: 0,
        error: { code: 0, message: errorMessage },
      })
      const fail = (errorMessage: string) => {
        return route.fulfill({ body: JSON.stringify(toJSONRPCError(errorMessage)) })
      }
      if (request.method() !== 'POST') {
        return route.fulfill({ status: 400 })
      }
      const cliArgumentsRaw = JSON.parse(
        new URL(request.url()).searchParams.get('cli-arguments') ?? '[]',
      )
      const cliArgumentsObject =
        cliArgumentsRaw[0] != null ?
          { name: cliArgumentsRaw[0].slice(2), arguments: cliArgumentsRaw.slice(1) }
        : null
      if (!cliArgumentsObject) {
        return fail('Missing arguments object')
      }
      const cliArguments: readonly string[] = cliArgumentsObject.arguments
      switch (cliArgumentsObject.name) {
        case 'filesystem-exists': {
          const path = cliArguments[0]
          if (path == null) {
            return fail('No path provided')
          }
          return succeed({ exists: fileSystem.has(path) })
        }
        case 'filesystem-list':
        case 'filesystem-list-recursive': {
          const recursive = cliArgumentsObject.name === 'filesystem-list-recursive'
          const folderPath = cliArguments[0]?.replace(/[/]$/, '')
          const folder = folderPath != null ? fileSystem.get(folderPath) : null
          if (folderPath == null || folder?.type !== 'DirectoryEntry') {
            return fail(`Could not find folder at '${folderPath}'`)
          }
          const folderPathQueue = [folderPath]
          const entries: FileSystemEntry[] = []
          while (true) {
            const currentFolderPath = folderPathQueue.shift()
            if (currentFolderPath == null) break
            for (const { entry } of folder.children) {
              if (recursive && entry.type === 'DirectoryEntry') {
                folderPathQueue.push(entry.path)
              }
              entries.push(entry)
            }
          }
          return succeed({ entries })
        }
        case 'filesystem-read-path': {
          const filePath = cliArguments[0]
          if (filePath == null) {
            return fail('No path provided')
          }
          called('getFileContent', { path: filePath })
          const file = fileSystem.get(filePath)
          if (!file) {
            return fail(`Could not find file at '${filePath}'`)
          }
          if (file.type !== 'FileEntry') {
            return fail(`Filesystem entry at '${filePath}' is '${file?.type}', not file`)
          }
          return succeed(file.content, 'text/plain')
        }
        case 'filesystem-create-directory': {
          const folderPath = cliArguments[0]
          if (folderPath == null) {
            return fail('No path provided')
          }
          const folder = fileSystem.get(folderPath)
          if (folder) {
            return fail(`Item already exists at '${folderPath}'`)
          }
          addDirectory({ path: Path(folderPath) })
          return succeed({})
        }
        case 'filesystem-write-path': {
          const filePath = cliArguments[0]
          if (filePath == null) {
            return fail('No path provided')
          }
          // Remove any existing file, if one exists.
          removeEntry(Path(filePath))
          addFile({ path: Path(filePath), content: request.postData() ?? '' })
          return succeed({})
        }
        case 'filesystem-move-from': {
          if (cliArguments[1] !== '--filesystem-move-to') {
            return fail('`--filesystem-move-to` not found')
          }
          const sourcePath = cliArguments[0]
          const destinationPath = cliArguments[2]
          if (sourcePath == null) {
            return fail('No source path provided')
          }
          if (destinationPath == null) {
            return fail('No destination path provided')
          }
          moveEntry(Path(sourcePath), Path(destinationPath))
          return succeed({})
        }
        case 'filesystem-delete': {
          const path = cliArguments[0]
          if (path == null) {
            return fail('No path provided')
          }
          const entry = fileSystem.get(path)
          if (!entry) {
            return fail(`Item does not exist at '${path}'`)
          }
          removeEntry(Path(path))
          return succeed({})
        }
      }
    })
  })

  async function updateVisualization(preprocessor: string, data: unknown) {
    for (const update of makeVisUpdates(preprocessor, data)) {
      languageServerBinaryWs?.send(Buffer.from(update))
    }
  }

  const api = {
    rootPath: ROOT_PATH,
    trackCalls,
    addDirectory,
    addProject,
    addFile,
    removeEntry,
    updateVisualization,
  } as const

  return api
}
