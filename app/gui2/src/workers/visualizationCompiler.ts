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
 * - (begin `compileVisualization`) The Worker `fetch`es the path.
 * - The CSS styles are compiled using `vue/compiler-sfc`, then sent as `AddStyleNotification`s.
 * - The Vue script is compiled using `vue/compiler-sfc` into TypeScript.
 * - The TypeScript is compiled using `sucrase` into JavaScript.
 * - (`rewriteImports`) Imports are analyzed and rewritten as required:
 *   - (`importSvg`) SVG imports are fetched and sent using an `AddRawImportNotification`.
 *   - (`importVue`) Vue imports are recursively compiled as described in this process.
 *   - (`importTS`) TypeScript imports are recursively compiled as described in this process,
 *     excluding the style and script compilation steps.
 * - (end `compileVisualization`) An `AddUrlNotification` with path `/Viz.vue` is sent to the main
 *   thread.
 * - A `CompilationResultResponse` with id `1` and path `/Viz.vue` is sent to the main thread.
 *
 * # Protocol
 *
 * ## Main Thread to Worker
 *
 * ### Compile
 * This is a request to compile a visualization module. The Worker MUST reply with a
 * `CompilationResultResponse` when compilation is done. The `id` is an arbitrary number that
 * uniquely identifies the request.
 * The `path` is either an absolute URL (`http://doma.in/path/to/TheScript.vue`), or a root-relative
 * URL (`/visualizations/TheScript.vue`). Relative URLs (`./TheScript.vue`) are NOT valid.
 *
 * Note that compiling files other than Vue files (TypeScript, SVG etc.) are currently NOT
 * supported.
 * ```ts
 * interface CompileRequest {
 *   type: 'compile'
 *   id: number
 *   path: string
 * }
 * ```
 *
 * ### Register Builtin Modules
 * This is a request to mark modules as built-in, indicating that the compiler should re-write the
 * imports into object destructures.
 * ```ts
 * interface RegisterBuiltinModulesRequest {
 *   type: 'register-builtin-modules'
 *   modules: string[]
 * }
 * ```
 *
 * ## Worker to Main Thread
 *
 * ### Compilation Result
 * Sent in response to a `CompileRequest`, with an `id` matching the `id` of the original request.
 * Contains only the `path` of the resulting file (which should have also been sent in the
 * `CompileRequest`). The content itself will have been sent earlier as an `AddImportNotification`.
 * ```ts
 * interface CompilationResultResponse {
 *   type: 'compilation-result'
 *   id: number
 *   path: string
 * }
 *
 * ### Add Style
 * Sent after compiling `<style>` and `<style scoped>` sections. These should be attached to the
 * DOM - placement does not matter.
 * ```ts
 * interface AddStyleNotification {
 *   type: 'style'
 *   code: string
 * }
 * ```
 *
 * ### Add Raw Import
 * Currently unused.
 *
 * Sent after compiling an import which does not result in a URL.
 *
 * Should be added to the cache using `cache[path] = value`.
 * ```ts
 * interface AddRawImportNotification {
 *   type: 'raw-import'
 *   path: string
 *   value: unknown
 * }
 * ```
 *
 * ### Add URL Import
 * Sent after compiling an import which results in a URL as its default export.
 * This is usually the case for assets.
 *
 * Should be added to the cache using `cache[path] = { default: URL.createObjectURL(new Blob([value], { type: mimeType })) }`.
 * ```ts
 * interface AddURLImportNotification {
 *   type: 'url-import'
 *   path: string
 *   mimeType: string
 *   value: BlobPart
 * }
 * ```
 *
 * ### Add Import
 * Sent after compiling a JavaScript import.
 *
 * Should be added to the cache using `cache[path] = import(URL.createObjectURL(new Blob([code], { type: 'text/javascript' })))`.
 * ```ts
 * interface AddImportNotification {
 *   type: 'import'
 *   path: string
 *   code: string
 * }
 * ```
 */

import { parse, compileScript, compileStyle } from 'vue/compiler-sfc'
import { transform } from 'sucrase'
import { parse as babelParse } from '@babel/parser'
import MagicString from 'magic-string'

let builtinModules = new Set<string>()
const ids = new Set<string>()
function generateId() {
  for (;;) {
    const id = Math.floor(Math.random() * 0xffffffff)
      .toString(16)
      .padStart(8, '0')
    if (!ids.has(id)) {
      return id
    }
  }
}

function addStyle(code: string) {
  postMessage({ type: 'style', code })
}
// function addRawImport(path: string, value: unknown) {
//   postMessage({ type: 'raw-import', path, value })
// }
function addUrlImport(path: string, mimeType: string, value: BlobPart) {
  postMessage({ type: 'url-import', path, mimeType, value })
}
function addImport(path: string, code: string) {
  postMessage({ type: 'import', path, code })
}

async function importSvg(path: string) {
  const text = await (await fetch(path)).text()
  addUrlImport(path, 'image/svg+xml', text)
}

async function importTS(path: string) {
  const dir = path.replace(/[^/\\]+$/, '')
  const scriptTs = await (await fetch(path)).text()
  const text = transform(scriptTs, {
    disableESTransforms: true,
    transforms: ['typescript'],
  }).code
  addImport(path, await rewriteImports(text, dir, undefined))
}

async function importVue(path: string) {
  const dir = path.replace(/[^/\\]+$/, '')
  const raw = await (await fetch(path)).text()
  const filename = path.match(/[^/\\]+$/)?.[0]!
  const parsed = parse(raw, { filename })
  const id = generateId()
  for (const style of parsed.descriptor.styles) {
    addStyle(
      compileStyle({ filename, source: style.content, id, scoped: style.scoped ?? false }).code,
    )
  }
  const scriptTs = compileScript(parsed.descriptor, {
    id,
    inlineTemplate: true,
    sourceMap: false,
  }).content
  const text = transform(scriptTs, {
    disableESTransforms: true,
    transforms: ['typescript'],
  }).code
  addImport(path, await rewriteImports(text, dir, id))
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
        if (
          isBuiltin ||
          (isRelative && (path.endsWith('.svg') || path.endsWith('.ts') || path.endsWith('.vue')))
        ) {
          const specifiers = stmt.specifiers.map((s: any) => {
            if (s.type === 'ImportDefaultSpecifier') {
              return `default: ${s.local.name}`
            } else {
              if (s.imported.start === s.local.start) {
                return s.imported.loc.identifierName
              } else {
                return `${s.imported.loc.identifierName}: ${s.local.loc.identifierName}`
              }
            }
          })
          s.overwrite(
            stmt.start!,
            stmt.end!,
            `const { ${specifiers.join(
              ', ',
            )} } = await window.__visualizationModules[${JSON.stringify(path)}];`,
          )
          if (isBuiltin) {
            // No further action is needed.
          } else if (path.endsWith('.svg')) {
            await importSvg(path)
          } else if (path.endsWith('.ts')) {
            await importTS(path)
          } else if (path.endsWith('.vue')) {
            await importVue(path)
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

async function compileVisualization(path: string, addStyle: (code: string) => void) {
  const filename = path.match(/[^/\\]+$/)?.[0]!
  const dir = path.replace(/[^/\\]+$/, '')
  const text = await (await fetch(path)).text()
  const id = generateId()
  const parsed = parse(text, { filename })
  for (const style of parsed.descriptor.styles) {
    addStyle(
      compileStyle({ filename, source: style.content, id, scoped: style.scoped ?? false }).code,
    )
  }
  const scriptTs = compileScript(parsed.descriptor, {
    id,
    inlineTemplate: true,
    sourceMap: false,
  }).content
  const scriptRaw = transform(scriptTs, {
    disableESTransforms: true,
    transforms: ['typescript'],
  }).code
  addImport(path, await rewriteImports(scriptRaw, dir, id))
  return path
}

onmessage = async (
  event: MessageEvent<
    | { type: 'register-builtin-modules'; modules: string[] }
    | { type: 'compile'; id: number; path: string }
  >,
) => {
  switch (event.data.type) {
    case 'register-builtin-modules': {
      builtinModules = new Set(event.data.modules)
      break
    }
    case 'compile': {
      postMessage({
        type: 'compilation-result',
        id: event.data.id,
        path: await compileVisualization(event.data.path, addStyle),
      })
      break
    }
  }
}
