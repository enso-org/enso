import VisualizationContainer from '@/components/VisualizationContainer.vue'
import { useVisualizationConfig } from '@/providers/visualizationConfig'
import type { Visualization } from '@/stores/visualization'
import type {
  AddImportNotification,
  AddRawImportNotification,
  AddStyleNotification,
  AddURLImportNotification,
  CompilationErrorResponse,
  CompilationResultResponse,
  CompileError,
  CompileRequest,
  FetchError,
  FetchResultWorkerResponse,
  FetchWorkerError,
  FetchWorkerRequest,
  InvalidMimetypeError,
  RegisterBuiltinModulesRequest,
} from '@/stores/visualization/compiler'
import Compiler from '@/stores/visualization/compiler?worker'
import { assertNever } from '@/util/assert'
import type { Opt } from '@/util/opt'
import { defineKeybinds } from '@/util/shortcuts'
import type { DataServer } from 'shared/dataServer'
import type { Uuid } from 'shared/languageServerTypes'
import * as vue from 'vue'
import { z } from 'zod'

const VisualizationModule = z.object({
  // This is UNSAFE, but unavoiable as the type of `Visualization` is too difficult to statically
  // check.
  default: z.custom<Visualization>(() => true),
  name: z.string(),
  inputType: z.string().optional(),
  defaultPreprocessor: (
    z.string().array().min(2) as unknown as z.ZodType<
      [module: string, method: string, ...args: string[]]
    >
  )
    .readonly()
    .optional(),
  scripts: z.string().array().optional(),
  styles: z.string().array().optional(),
})
export type VisualizationModule = z.infer<typeof VisualizationModule>

const moduleCache: Record<string, any> = {
  vue,
  get d3() {
    return import('d3')
  },
  builtins: { VisualizationContainer, useVisualizationConfig, defineKeybinds },
}
// @ts-expect-error Intentionally not defined in `env.d.ts` as it is a mistake to access this
// anywhere else.
window.__visualizationModules = moduleCache

export class InvalidVisualizationModuleError extends TypeError {
  constructor(public path: string) {
    super(`The module '${path}' is not a visualization.`)
  }
}

let worker: Worker | undefined
let workerMessageId = 0
const workerCallbacks = new Map<
  number,
  { resolve: (result: VisualizationModule) => void; reject: (error: Error) => void }
>()

function postMessage<T>(worker: Worker, message: T) {
  worker.postMessage(message)
}

export async function compile(path: string, projectRoot: Opt<Uuid>, data: DataServer) {
  if (worker == null) {
    const worker_ = (worker = new Compiler())
    postMessage<RegisterBuiltinModulesRequest>(worker, {
      type: 'register-builtin-modules-request',
      modules: Object.keys(moduleCache),
    })
    worker.addEventListener(
      'message',
      async (
        event: MessageEvent<
          // === Responses ===
          | CompilationResultResponse
          | CompilationErrorResponse
          // === Worker Requests ===
          | FetchWorkerRequest
          // === Notifications ===
          | AddStyleNotification
          | AddRawImportNotification
          | AddURLImportNotification
          | AddImportNotification
          // === Errors ===
          | FetchError
          | InvalidMimetypeError
          | CompileError
        >,
      ) => {
        switch (event.data.type) {
          // === Responses ===
          case 'compilation-result-response': {
            try {
              const module = await moduleCache[event.data.path]
              const vizModule = VisualizationModule.parse(module)
              if (vizModule) {
                workerCallbacks.get(event.data.id)?.resolve(vizModule)
              } else {
                workerCallbacks
                  .get(event.data.id)
                  ?.reject(new InvalidVisualizationModuleError(event.data.path))
              }
            } catch (error: any) {
              workerCallbacks.get(event.data.id)?.reject(error)
            }
            workerCallbacks.delete(event.data.id)
            break
          }
          case 'compilation-error-response': {
            console.error(`Error compiling visualization '${event.data.path}':`, event.data.error)
            workerCallbacks.get(event.data.id)?.reject(event.data.error)
            workerCallbacks.delete(event.data.id)
            break
          }
          // === Worker Requests ===
          case 'fetch-worker-request': {
            try {
              const url = new URL(event.data.path, location.href)
              switch (url.protocol) {
                case 'http:':
                case 'https:': {
                  const response = await fetch(url)
                  if (response.ok) {
                    postMessage<FetchResultWorkerResponse>(worker_, {
                      type: 'fetch-result-worker-response',
                      path: event.data.path,
                      contents: await response.arrayBuffer(),
                      contentType: response.headers.get('Content-Type') ?? undefined,
                    })
                  } else {
                    postMessage<FetchWorkerError>(worker_, {
                      type: 'fetch-worker-error',
                      path: event.data.path,
                      error: new Error(
                        `\`fetch\` returned status code ${response.status} (${response.statusText})`,
                      ),
                    })
                  }
                  break
                }
                case 'enso-current-project:': {
                  const rootId = projectRoot
                  if (!rootId) {
                    postMessage<FetchWorkerError>(worker_, {
                      type: 'fetch-worker-error',
                      path: event.data.path,
                      error: new Error(
                        'Could not find a file system content root for the current project.',
                      ),
                    })
                    break
                  }
                  const payload = await data.readFile({ rootId, segments: url.pathname.split('/') })
                  const contents = payload.contentsArray()
                  if (!contents) {
                    postMessage<FetchWorkerError>(worker_, {
                      type: 'fetch-worker-error',
                      path: event.data.path,
                      error: new Error(
                        `An invalid response was recieved when fetching '${url.pathname}' from the current project.`,
                      ),
                    })
                    break
                  }
                  postMessage<FetchResultWorkerResponse>(worker_, {
                    type: 'fetch-result-worker-response',
                    path: event.data.path,
                    contents,
                    contentType: undefined,
                  })
                  break
                }
              }
            } catch (error) {
              postMessage<FetchWorkerError>(worker_, {
                type: 'fetch-worker-error',
                path: event.data.path,
                error: error instanceof Error ? error : new Error(`${error}`),
              })
            }
            break
          }
          // === Notifications ===
          case 'add-style-notification': {
            const styleNode = document.createElement('style')
            styleNode.innerHTML = event.data.code
            document.head.appendChild(styleNode)
            break
          }
          case 'add-raw-import-notification': {
            // FIXME: include protocol in path so that custom visualizations and their
            // dependencies do not overwrite builtin visualizations.
            moduleCache[event.data.path] = event.data.value
            break
          }
          case 'add-url-import-notification': {
            moduleCache[event.data.path] = {
              default: URL.createObjectURL(
                new Blob([event.data.value], { type: event.data.mimeType }),
              ),
            }
            break
          }
          case 'add-import-notification': {
            try {
              const module = import(
                /* @vite-ignore */
                URL.createObjectURL(new Blob([event.data.code], { type: 'text/javascript' }))
              )
              // Required for 'compilation-result-response' handler above.
              moduleCache[event.data.path] = module
              moduleCache[event.data.path] = await module
            } catch {
              // Ignored - the same promise is awaited elsewhere.
            }
            break
          }
          // === Errors ===
          case 'fetch-error': {
            console.error(`Error fetching '${event.data.path}':`, event.data.error)
            break
          }
          case 'invalid-mimetype-error': {
            console.error(
              `Expected mimetype of '${event.data.path}' to be '${event.data.expected}', ` +
                `but received '${event.data.actual}' instead`,
            )
            break
          }
          case 'compile-error': {
            console.error(`Error compiling '${event.data.path}':`, event.data.error)
            break
          }
          default: {
            assertNever(event.data)
          }
        }
      },
    )
    worker.addEventListener('error', (event) => console.error(event.error))
  }
  const id = workerMessageId
  workerMessageId += 1
  const promise = new Promise<VisualizationModule>((resolve, reject) => {
    workerCallbacks.set(id, { resolve, reject })
  })
  postMessage<CompileRequest>(worker, { type: 'compile-request', id, path })
  return await promise
}
