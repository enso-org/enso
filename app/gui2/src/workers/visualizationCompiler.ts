import { parse, compileScript, compileStyle } from 'vue/compiler-sfc'
import { transform } from 'sucrase'
import { parse as babelParse } from '@babel/parser'
import MagicString from 'magic-string'

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
function addUrlImport(path: string, mimeType: string, value: string) {
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
  for (let i = 0; i < ast.program.body.length; ) {
    const stmt = ast.program.body[i]
    switch (stmt.type) {
      case 'ImportDeclaration': {
        let path = stmt.source.extra!.rawValue as string
        const isRelative = /^[./]/.test(path)
        if (isRelative) {
          path = new URL(dir + path, location.href).toString()
        }
        if (
          path == 'vue' ||
          path.endsWith('.svg') ||
          path.endsWith('.ts') ||
          path.endsWith('.vue')
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
          if (path.endsWith('.svg')) {
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
        if (id != null && stmt.declaration?.callee?.name === '_defineComponent') {
          const firstProp = stmt.declaration?.arguments?.[0]?.properties?.[0]
          if (firstProp != null) {
            s.appendLeft(firstProp.start, `__scopeId: ${JSON.stringify(`data-v-${id}`)}, `)
          }
        }
        break
      }
    }
    i += 1
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

onmessage = async (event: MessageEvent<{ id: number; path: string }>) => {
  postMessage({
    type: 'script',
    id: event.data.id,
    path: await compileVisualization(event.data.path, addStyle),
  })
}
