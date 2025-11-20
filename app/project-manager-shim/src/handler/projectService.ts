import { Path, UUID } from 'enso-common/src/services/Backend'
import type * as http from 'node:http'
import { ProjectService, type CloudParams } from '../projectService/index.js'
import { toJSONRPCError, toJSONRPCResult } from './jsonrpc.js'

const HTTP_STATUS_OK = 200

/** Read JSON from an HTTP request body. */
async function bodyJson<T>(request: http.IncomingMessage): Promise<T> {
  const chunks: Buffer[] = []
  for await (const chunk of request) {
    chunks.push(typeof chunk === 'string' ? Buffer.from(chunk) : chunk)
  }
  const body = Buffer.concat(chunks).toString('utf-8')
  return JSON.parse(body) as T
}

/** Check if this is a project service request */
export function isProjectServiceRequest(requestPath: string): boolean {
  return requestPath.startsWith('/api/project-service/')
}

/** Handle project service requests */
export async function handleProjectServiceRequest(
  request: http.IncomingMessage,
  response: http.ServerResponse,
  requestPath: string,
  getProjectService: () => Promise<ProjectService>,
  headers: Record<string, string>,
): Promise<void> {
  switch (`${request.method} ${requestPath}`) {
    case 'POST /api/project-service/project/create': {
      interface ResponseBody {
        readonly name: string
        readonly projectsDirectory: Path
      }
      bodyJson<ResponseBody>(request)
        .then(async (body) => {
          const projectService = await getProjectService()
          return projectService.createProject(body.name, body.projectsDirectory)
        })
        .then((result) => {
          response.writeHead(HTTP_STATUS_OK, headers).end(toJSONRPCResult(result))
        })
        .catch((err) => {
          console.error(err)
          response
            .writeHead(HTTP_STATUS_OK, headers)
            .end(toJSONRPCError('project/create failed', err))
        })
      break
    }
    case 'POST /api/project-service/project/open': {
      interface Body {
        readonly projectId: UUID
        readonly projectsDirectory: Path
        readonly cloud?: CloudParams
      }
      bodyJson<Body>(request)
        .then(async (body) => {
          const projectService = await getProjectService()
          return projectService.openProject(body.projectId, body.projectsDirectory, body.cloud)
        })
        .then((result) => {
          response.writeHead(HTTP_STATUS_OK, headers).end(toJSONRPCResult(result))
        })
        .catch((err) => {
          console.error(err)
          response
            .writeHead(HTTP_STATUS_OK, headers)
            .end(toJSONRPCError('project/open failed', err))
        })
      break
    }
    case 'POST /api/project-service/project/close': {
      interface Body {
        readonly projectId: UUID
      }
      bodyJson<Body>(request)
        .then(async (body) => {
          const projectService = await getProjectService()
          return projectService.closeProject(body.projectId)
        })
        .then(() => {
          response.writeHead(HTTP_STATUS_OK, headers).end(toJSONRPCResult(null))
        })
        .catch((err) => {
          console.error(err)
          response
            .writeHead(HTTP_STATUS_OK, headers)
            .end(toJSONRPCError('project/close failed', err))
        })
      break
    }
    case 'POST /api/project-service/project/delete': {
      interface Body {
        readonly projectId: UUID
        readonly projectsDirectory: Path
      }
      bodyJson<Body>(request)
        .then(async (body) => {
          const projectService = await getProjectService()
          return projectService.deleteProject(body.projectId, body.projectsDirectory)
        })
        .then(() => {
          response.writeHead(HTTP_STATUS_OK, headers).end(toJSONRPCResult(null))
        })
        .catch((err) => {
          console.error(err)
          response
            .writeHead(HTTP_STATUS_OK, headers)
            .end(toJSONRPCError('project/delete failed', err))
        })
      break
    }
    case 'POST /api/project-service/project/duplicate': {
      interface Body {
        readonly projectId: UUID
        readonly projectsDirectory: Path
      }
      bodyJson<Body>(request)
        .then(async (body) => {
          const projectService = await getProjectService()
          return projectService.duplicateProject(body.projectId, body.projectsDirectory)
        })
        .then((result) => {
          response.writeHead(HTTP_STATUS_OK, headers).end(toJSONRPCResult(result))
        })
        .catch((err) => {
          console.error(err)
          response
            .writeHead(HTTP_STATUS_OK, headers)
            .end(toJSONRPCError('project/duplicate failed', err))
        })
      break
    }
    case 'POST /api/project-service/project/rename': {
      interface Body {
        readonly projectId: UUID
        readonly name: string
        readonly projectsDirectory: Path
      }
      bodyJson<Body>(request)
        .then(async (body) => {
          const projectService = await getProjectService()
          return projectService.renameProject(body.projectId, body.name, body.projectsDirectory)
        })
        .then(() => {
          response.writeHead(HTTP_STATUS_OK, headers).end(toJSONRPCResult(null))
        })
        .catch((err) => {
          console.error(err)
          response
            .writeHead(HTTP_STATUS_OK, headers)
            .end(toJSONRPCError('project/rename failed', err))
        })
      break
    }
    default: {
      throw new Error(`Unknown project service request ${requestPath}`)
    }
  }
}
