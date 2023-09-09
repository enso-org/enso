import { parse, compileScript, compileStyle } from 'vue/compiler-sfc'
import { transform } from 'sucrase'
import { parse as babelParse } from '@babel/parser'
import MagicString from 'magic-string'
import { first } from 'lib0/set.js'

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

async function rewriteSVGImport(path: string, s: MagicString, stmt: any) {
  const text = await (await fetch(path)).text()
  s.overwrite(
    stmt.start!,
    stmt.end!,
    `const ${stmt.specifiers[0].local.name} = "data:image/svg+xml,${encodeURIComponent(text)}";`,
  )
}

async function rewriteTSImport(
  path: string,
  s: MagicString,
  stmt: any,
  addStyle: (code: string) => void,
) {
  const dir = path.replace(/[^/\\]+$/, '')
  const scriptTs = await (await fetch(path)).text()
  const text = transform(scriptTs, {
    disableESTransforms: true,
    transforms: ['typescript'],
  }).code
  s.overwrite(
    stmt.source.start!,
    stmt.source.end!,
    `"data:text/javascript,${encodeURIComponent(
      await rewriteImports(text, dir, undefined, addStyle),
    )}"`,
  )
}

async function rewriteVueImport(
  path: string,
  s: MagicString,
  stmt: any,
  addStyle: (code: string) => void,
) {
  const dir = path.replace(/[^/\\]+$/, '')
  const raw = await (await fetch(path)).text()
  const filename = path.match(/[^/\\]+$/)?.[0]!
  const parsed = parse(raw, { filename })
  const id = generateId()
  for (const style of parsed.descriptor.styles) {
    addStyle(compileStyle({ filename, source: style.content, id, scoped: style.scoped }).code)
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
  s.overwrite(
    stmt.source.start!,
    stmt.source.end!,
    `"data:text/javascript,${encodeURIComponent(await rewriteImports(text, dir, id, addStyle))}"`,
  )
}

async function rewriteImports(
  code: string,
  dir: string,
  id: string | undefined,
  addStyle: (code: string) => void,
) {
  const ast = babelParse(code, { sourceType: 'module' })
  const s = new MagicString(code)
  for (let i = 0; i < ast.program.body.length; ) {
    const stmt = ast.program.body[i]
    switch (stmt.type) {
      case 'ImportDeclaration': {
        let path = stmt.source.extra!.rawValue as string
        const isRelative = path.startsWith('.')
        if (isRelative) {
          path = new URL(dir + path, location.href).toString()
        }
        if (path === 'vue') {
          const specifiers = stmt.specifiers.map((s: any) => {
            if (s.imported.start === s.local.start) {
              return s.imported.loc.identifierName
            } else {
              return `${s.imported.loc.identifierName}: ${s.local.loc.identifierName}`
            }
          })
          s.overwrite(stmt.start!, stmt.end!, `const { ${specifiers.join(', ')} } = window.vue;`)
        } else if (path.endsWith('.svg')) {
          await rewriteSVGImport(path, s, stmt)
        } else if (path.endsWith('.ts')) {
          await rewriteTSImport(path, s, stmt, addStyle)
        } else if (path.endsWith('.vue')) {
          await rewriteVueImport(path, s, stmt, addStyle)
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
    addStyle(compileStyle({ filename, source: style.content, id, scoped: style.scoped }).code)
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
  return await rewriteImports(scriptRaw, dir, id, addStyle)
}

onmessage = async (event: MessageEvent<{ id: number; path: string }>) => {
  function addStyle(code: string) {
    postMessage({ type: 'style', code })
  }
  postMessage({
    type: 'script',
    id: event.data.id,
    code: await compileVisualization(event.data.path, addStyle),
  })
}
