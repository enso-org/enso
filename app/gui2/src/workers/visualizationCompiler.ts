/**
 * This Web Worker compiles visualizations in a background thread.
 *
 * # High-Level Overview
 * Imports are recursively compiled.
 * - Unknown imports, are preserved as-is.
 * - Compiled imports are added to a cache on the main thread.
 * - Compiled imports are re-written into an object destructure, so that the cache can be used.
 * - Uses `compiler-sfc` to compile Vue files into TypeScript + CSS, then `sucrase` to compile
 * the resulting TypeScript into JavaScript.
 * - Uses `sucrase` to compile TypeScript files into JavaScript.
 * - Converts SVG files into imports in which the `default` export is the raw text of the SVG image.
 *
 * # Typical Request Lifetime
 * See the "Protocol" section below for details on specific messages.
 * - A `CompileRequest` is sent with id `1` and path `/Viz.vue`.
 * - (begin `importVue`) The Worker `fetch`es the path.
 * - The CSS styles are compiled using `vue/compiler-sfc`, then sent as `AddStyleNotification`s.
 * - The Vue script is compiled using `vue/compiler-sfc` into TypeScript.
 * - The TypeScript is compiled using `sucrase` into JavaScript.
 * - (`rewriteImports`) Imports are analyzed and rewritten as required:
 *   - (`importSvg`) SVG imports are fetched and sent using an `AddRawImportNotification`.
 *   - (`importVue`) Vue imports are recursively compiled as described in this process.
 *   - (`importTS`) TypeScript imports are recursively compiled as described in this process,
 *     excluding the style and script compilation steps.
 * - (end `importVue`) An `AddUrlNotification` with path `/Viz.vue` is sent to the main
 *   thread.
 * - A `CompilationResultResponse` with id `1` and path `/Viz.vue` is sent to the main thread. */

import { parse as babelParse } from '@babel/parser'
import hash from 'hash-sum'
import MagicString from 'magic-string'
import { transform } from 'sucrase'
import { compileScript, compileStyle, parse } from 'vue/compiler-sfc'

// ========================================
// === Requests (Main Thread to Worker) ===
// ========================================

/** A request to compile a visualization module. The Worker MUST reply with a
 * {@link CompilationResultResponse} when compilation is done, or a {@link CompilationErrorResponse}
 * when compilation fails. The `id` is an arbitrary number that uniquely identifies the request.
 * The `path` is either an absolute URL (`http://doma.in/path/to/TheScript.vue`), or a root-relative
 * URL (`/visualizations/TheScript.vue`). Relative URLs (`./TheScript.vue`) are NOT valid.
 *
 * Note that compiling files other than Vue files (TypeScript, SVG etc.) are currently NOT
 * supported. */
export interface CompileRequest {
  type: 'compile-request'
  id: number
  path: string
}

/** A request to mark modules as built-in, indicating that the compiler should re-write the imports
 * into object destructures. */
export interface RegisterBuiltinModulesRequest {
  type: 'register-builtin-modules-request'
  modules: string[]
}

// =========================================
// === Responses (Worker to Main Thread) ===
// =========================================

// These are messages sent in response to a query. They contain the `id` of the original query.

/** Sent in response to a {@link CompileRequest}, with an `id` matching the `id` of the original
 * request. Contains only the `path` of the resulting file (which should have also been sent in the
 * {@link CompileRequest}).
 * The content itself will have been sent earlier as an {@link AddImportNotification}. */
export interface CompilationResultResponse {
  type: 'compilation-result-response'
  id: number
  path: string
}

/** Sent in response to a {@link CompileRequest}, with an `id` matching the `id` of the original
 * request. Contains the `path` of the resulting file (which should have also been sent in the
 * {@link CompileRequest}), and the `error` thrown during compilation. */
export interface CompilationErrorResponse {
  type: 'compilation-error-response'
  id: number
  path: string
  error: Error
}

// =============================================
// === Notifications (Worker to Main Thread) ===
// =============================================

// These are sent when a subtask successfully completes execution.

/** Sent after compiling `<style>` and `<style scoped>` sections.
 * These should be attached to the DOM - placement does not matter. */
export interface AddStyleNotification {
  type: 'add-style-notification'
  code: string
}

/** Currently unused.
 *
 * Sent after compiling an import which does not result in a URL.
 *
 * Should be added to the cache using `cache[path] = value`. */
export interface AddRawImportNotification {
  type: 'add-raw-import-notification'
  path: string
  value: unknown
}

/** Sent after compiling an import which results in a URL as its default export.
 * This is usually the case for assets.
 *
 * Should be added to the cache using
 * `cache[path] = { default: URL.createObjectURL(new Blob([value], { type: mimeType })) }`. */
export interface AddURLImportNotification {
  type: 'add-url-import-notification'
  path: string
  mimeType: string
  value: BlobPart
}

/** Sent after compiling a JavaScript import.
 *
 * Should be added to the cache using
 * `cache[path] = import(URL.createObjectURL(new Blob([code], { type: 'text/javascript' })))`. */
export interface AddImportNotification {
  type: 'add-import-notification'
  path: string
  code: string
}

// ======================================
// === Errors (Worker to Main Thread) ===
// ======================================

// These are sent when a subtask fails to complete execution.

/** Sent when the `fetch` call errored, or returned a failure HTTP status code (any code other than
 * 200-299). */
export interface FetchError {
  type: 'fetch-error'
  path: string
  error: Error
}

/** Sent when the `fetch` call succeeded, but returned a response with an unexpected type. */
export interface InvalidMimetypeError {
  type: 'invalid-mimetype-error'
  path: string
  expected: string
  actual: string
}

/** Sent when compilation of a TypeScript or Vue script failed. */
export interface CompileError {
  type: 'compile-error'
  path: string
  error: Error
}

// ================
// === Compiler ===
// ================

let builtinModules = new Set<string>()
const alreadyCompiledModules = new Set<string>()

const assetMimetypes: Record<string, string> = {
  // === Image formats ===
  svg: 'image/svg+xml',
  png: 'image/png',
  jpg: 'image/jpeg',
  jpeg: 'image/jpeg',
  jpe: 'image/jpeg',
  jif: 'image/jpeg',
  jfif: 'image/jpeg',
  jfi: 'image/jpeg',
  webp: 'image/webp',
  gif: 'image/gif',
  apng: 'image/apng',

  // === Audio formats ===
  ogg: 'audio/ogg',
  opus: 'audio/ogg',
  oga: 'audio/ogg',
  flac: 'audio/flac',
  mp3: 'audio/mpeg',
  m4a: 'audio/mp4',
  m4p: 'audio/mp4',
  m4b: 'audio/mp4',
  m4r: 'audio/mp4',

  // === Video formats ===
  ogv: 'video/ogg',
  webm: 'video/webm',
  mov: 'video/quicktime',
  qt: 'video/quicktime',
  mp4: 'video/mp4',
  m4v: 'video/mp4',
  '3gp': 'video/3gpp',
  '3g2': 'video/3gpp2',
}

function extractExtension(path: string) {
  return path.match(/(?<=^|[.])[^.]+?(?=[#?]|$)/)?.[0] ?? ''
}

const postMessage: <T>(message: T) => void = globalThis.postMessage

function addStyle(code: string) {
  postMessage<AddStyleNotification>({ type: 'add-style-notification', code })
}
// This is defined to allow for future expansion.
// eslint-disable-next-line @typescript-eslint/no-unused-vars
function addRawImport(path: string, value: unknown) {
  postMessage<AddRawImportNotification>({ type: 'add-raw-import-notification', path, value })
}
function addUrlImport(path: string, mimeType: string, value: BlobPart) {
  postMessage<AddURLImportNotification>({
    type: 'add-url-import-notification',
    path,
    mimeType,
    value,
  })
}
function addImport(path: string, code: string) {
  postMessage<AddImportNotification>({ type: 'add-import-notification', path, code })
}

function fetchError(path: string, error: Error) {
  postMessage<FetchError>({ type: 'fetch-error', path, error })
}
function invalidMimetypeError(path: string, expected: string, actual: string) {
  postMessage<InvalidMimetypeError>({ type: 'invalid-mimetype-error', path, expected, actual })
}
function compileError(path: string, error: Error) {
  postMessage<CompileError>({ type: 'compile-error', path, error })
}

async function tryFetch(path: string) {
  try {
    const response = await fetch(path)
    if (!response.ok) {
      fetchError(
        path,
        new Error(`\`fetch\` returned status code ${response.status} (${response.statusText})`),
      )
    }
    return response
  } catch (error: any) {
    fetchError(path, error)
    return new Response()
  }
}

async function importAsset(path: string, mimeType: string) {
  const response = await tryFetch(path)
  const actualMimeType = response.headers.get('Content-Type')?.toLowerCase()
  if (actualMimeType != null && actualMimeType != mimeType) {
    invalidMimetypeError(path, mimeType, actualMimeType)
    return
  }
  const blob = await response.blob()
  addUrlImport(path, mimeType, blob)
}

async function importTS(path: string) {
  const dir = path.replace(/[^/\\]+$/, '')
  const scriptTs = await (await tryFetch(path)).text()
  let text: string | undefined
  try {
    text = transform(scriptTs, {
      disableESTransforms: true,
      transforms: ['typescript'],
    }).code
  } catch (error: any) {
    compileError(path, error)
  }
  if (text != null) {
    addImport(path, await rewriteImports(text, dir, undefined))
  }
}

async function importVue(path: string) {
  const dir = path.replace(/[^/\\]+$/, '')
  const raw = await (await tryFetch(path)).text()
  const filename = path.match(/[^/\\]+$/)?.[0]!
  const parsed = parse(raw, { filename })
  const id = hash(raw)
  for (const style of parsed.descriptor.styles) {
    addStyle(
      compileStyle({ filename, source: style.content, id, scoped: style.scoped ?? false }).code,
    )
  }
  let text: string | undefined
  try {
    const scriptTs = compileScript(parsed.descriptor, {
      id,
      inlineTemplate: true,
      sourceMap: false,
    }).content
    text = transform(scriptTs, {
      disableESTransforms: true,
      transforms: ['typescript'],
    }).code
  } catch (error: any) {
    compileError(path, error)
  }
  if (text != null) {
    addImport(path, await rewriteImports(text, dir, id))
  }
}

async function rewriteImports(code: string, dir: string, id: string | undefined) {
  const ast = babelParse(code, { sourceType: 'module' })
  const s = new MagicString(code)
  for (const stmt of ast.program.body) {
    switch (stmt.type) {
      case 'ImportDeclaration': {
        let path = stmt.source.extra!.rawValue as string
        const isBuiltin = builtinModules.has(path)
        const isRelative = /^[./]/.test(path)
        if (isRelative) {
          path = new URL(dir + path, location.href).toString()
        }
        const extension = isRelative ? extractExtension(path).toLowerCase() : ''
        if (
          isBuiltin ||
          (isRelative && (extension in assetMimetypes || extension === 'ts' || extension === 'vue'))
        ) {
          let namespace: string | undefined
          const specifiers = stmt.specifiers.flatMap((s: any) => {
            if (s.type === 'ImportDefaultSpecifier') {
              return [`default: ${s.local.name}`]
            } else if (s.type === 'ImportNamespaceSpecifier') {
              namespace = s.local.name
              return []
            } else {
              if (s.imported.start === s.local.start) {
                return [s.imported.loc.identifierName]
              } else {
                return [`${s.imported.loc.identifierName}: ${s.local.loc.identifierName}`]
              }
            }
          })
          const pathJSON = JSON.stringify(path)
          const destructureExpression = `{ ${specifiers.join(', ')} }`
          const rewritten =
            namespace != null
              ? `const ${namespace} = await window.__visualizationModules[${pathJSON}];` +
                (specifiers.length > 0 ? `\nconst ${destructureExpression} = ${namespace};` : '')
              : `const ${destructureExpression} = await window.__visualizationModules[${pathJSON}];`
          s.overwrite(stmt.start!, stmt.end!, rewritten)
          if (isBuiltin) {
            // No further action is needed.
          } else {
            if (alreadyCompiledModules.has(path)) {
              continue
            }
            alreadyCompiledModules.add(path)
            switch (extension) {
              case 'ts': {
                await importTS(path)
                break
              }
              case 'vue': {
                await importVue(path)
                break
              }
              default: {
                const mimetype = assetMimetypes[extension]
                if (mimetype != null) {
                  await importAsset(path, mimetype)
                  break
                }
              }
            }
          }
        }
        break
      }
      case 'ExportDefaultDeclaration': {
        if (id != null && (stmt.declaration as any)?.callee?.name === '_defineComponent') {
          const firstProp = (stmt.declaration as any)?.arguments?.[0]?.properties?.[0]
          if (firstProp?.start != null) {
            s.appendLeft(firstProp.start, `__scopeId: ${JSON.stringify(`data-v-${id}`)}, `)
          }
        }
        break
      }
    }
  }
  return s.toString()
}

onmessage = async (event: MessageEvent<RegisterBuiltinModulesRequest | CompileRequest>) => {
  switch (event.data.type) {
    case 'register-builtin-modules-request': {
      builtinModules = new Set(event.data.modules)
      break
    }
    case 'compile-request': {
      try {
        if (!alreadyCompiledModules.has(event.data.path)) {
          alreadyCompiledModules.add(event.data.path)
          await importVue(event.data.path)
        }
        postMessage<CompilationResultResponse>({
          type: 'compilation-result-response',
          id: event.data.id,
          path: event.data.path,
        })
      } catch (error: any) {
        postMessage<CompilationErrorResponse>({
          type: 'compilation-error-response',
          id: event.data.id,
          path: event.data.path,
          error,
        })
      }
      break
    }
  }
}
