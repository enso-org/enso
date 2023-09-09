import * as vue from 'vue'
import { parse, compileScript, compileStyle } from 'vue/compiler-sfc'
import { transform } from 'sucrase'
import { parse as babelParse } from '@babel/parser'
import MagicString from 'magic-string'

import { defineStore } from 'pinia'

// @ts-expect-error
window.vue = vue

type VisualizationModule =
  typeof import('../../public/visualizations/VisualizationContainer.vue') & {
    name?: string
    inputType?: string
    scripts?: string[]
    styles?: string[]
  }

export type Visualization = VisualizationModule['default']

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

async function rewriteTSImport(path: string, s: MagicString, stmt: any) {
  const dir = path.replace(/[^/\\]+$/, '')
  const scriptTs = await (await fetch(path)).text()
  const text = transform(scriptTs, {
    disableESTransforms: true,
    transforms: ['typescript'],
  }).code
  s.overwrite(
    stmt.source.start!,
    stmt.source.end!,
    `"data:text/javascript,${encodeURIComponent(await rewriteImports(text, dir))}"`,
  )
}

async function rewriteVueImport(path: string, s: MagicString, stmt: any) {
  const dir = path.replace(/[^/\\]+$/, '')
  const raw = await (await fetch(path)).text()
  const filename = path.match(/[^/\\]+$/)?.[0]!
  const parsed = parse(raw, { filename })
  const id = generateId()
  for (const style of parsed.descriptor.styles) {
    const css = compileStyle({ filename, source: style.content, id, scoped: style.scoped }).code
    const styleNode = document.createElement('style')
    styleNode.innerHTML = css
    document.head.appendChild(styleNode)
  }
  const scriptTs = compileScript(parsed.descriptor, {
    id,
    inlineTemplate: true,
    sourceMap: false,
    templateOptions: { id, scoped: true },
  }).content
  const text = transform(scriptTs, {
    disableESTransforms: true,
    transforms: ['typescript'],
  }).code
  s.overwrite(
    stmt.source.start!,
    stmt.source.end!,
    `"data:text/javascript,${encodeURIComponent(await rewriteImports(text, dir))}"`,
  )
}

async function rewriteImports(code: string, dir: string) {
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
          await rewriteTSImport(path, s, stmt)
        } else if (path.endsWith('.vue')) {
          await rewriteVueImport(path, s, stmt)
        }
        break
      }
    }
    i += 1
  }
  return s.toString()
}

export const useVisualizationStore = defineStore('visualization', () => {
  // FIXME: statically resolved imports will not work for user-defined components
  const paths: Record<string, string> = {
    JSON: '/visualizations/JSONVisualization.vue',
    Error: '/visualizations/ErrorVisualization.vue',
    Warnings: '/visualizations/WarningsVisualization.vue',
    Bubble: '/visualizations/BubbleVisualization.vue',
    Image: '/visualizations/ImageBase64Visualization.vue',
    'Geo Map': '/visualizations/GeoMapVisualization.vue',
    Scatterplot: '/visualizations/ScatterplotVisualization.vue',
  } as any
  let cache: Record<string, any> = {}
  const types = Object.keys(paths)

  function register(name: string, inputType: string) {
    console.log(`registering visualization: name=${name}, inputType=${inputType}`)
  }

  // NOTE: Because visualization scripts are cached, they are not guaranteed to be up to date.
  async function get(type: string) {
    let component: VisualizationModule['default'] = cache[type]
    if (component == null) {
      const path = paths[type]
      const filename = path.match(/[^/\\]+$/)?.[0]!
      const dir = path.replace(/[^/\\]+$/, '')
      const text = await (await fetch(path)).text()
      const id = generateId()
      const parsed = parse(text, { filename })
      for (const style of parsed.descriptor.styles) {
        const css = compileStyle({ filename, source: style.content, id, scoped: style.scoped }).code
        const styleNode = document.createElement('style')
        styleNode.innerHTML = css
        document.head.appendChild(styleNode)
      }
      const scriptTs = compileScript(parsed.descriptor, {
        id,
        inlineTemplate: true,
        sourceMap: false,
        templateOptions: { id, scoped: true },
      }).content
      const scriptRaw = transform(scriptTs, {
        disableESTransforms: true,
        transforms: ['typescript'],
      }).code
      const script = await rewriteImports(scriptRaw, dir)
      const module = await import(
        /* @vite-ignore */ `data:text/javascript,${encodeURIComponent(script)}`
      )
      // TODO[sb]: fallback to name based on path to visualization.
      register(module.name ?? type, module.inputType ?? 'Any')
      component = module.default
      cache[type] = component
    }
    return component
  }

  function clear() {
    cache = {}
  }

  function sampleData(type: string) {
    switch (type) {
      case 'Error': {
        return { kind: 'Dataflow', message: 'a data flow error' }
      }
      case 'Warnings': {
        return ['warning 1', "warning 2!!&<>;'\x22"]
      }
      case 'Bubble': {
        return [
          [10, 10, 10],
          [10, 100, 10],
          [100, 10, 10],
          [100, 100, 10],
          [25, 25, 5],
          [25, 85, 5],
          [85, 25, 5],
          [85, 85, 5],
          [55, 55, 20],
        ]
      }
      case 'Image': {
        return {
          mediaType: 'image/svg+xml',
          base64: `PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI0MCIgaGVpZ2h0PSI0\
MCI+PGcgY2xpcC1wYXRoPSJ1cmwoI2EpIj48cGF0aCBkPSJNMjAuMDUgMEEyMCAyMCAwIDAgMCAwIDIwLjA1IDIwLjA2IDIwLjA\
2IDAgMSAwIDIwLjA1IDBabTAgMzYuMDVjLTguOTMgMC0xNi4xLTcuMTctMTYuMS0xNi4xIDAtOC45NCA3LjE3LTE2LjEgMTYuMS\
0xNi4xIDguOTQgMCAxNi4xIDcuMTYgMTYuMSAxNi4xYTE2LjE4IDE2LjE4IDAgMCAxLTE2LjEgMTYuMVoiLz48cGF0aCBkPSJNM\
jcuMTIgMTcuNzdhNC42OCA0LjY4IDAgMCAxIDIuMzkgNS45MiAxMC4yMiAxMC4yMiAwIDAgMS05LjU2IDYuODZBMTAuMiAxMC4y\
IDAgMCAxIDkuNzcgMjAuMzZzMS41NSAyLjA4IDQuNTcgMi4wOGMzLjAxIDAgNC4zNi0xLjE0IDUuNi0yLjA4IDEuMjUtLjkzIDI\
uMDktMyA1LjItMyAuNzMgMCAxLjQ2LjIgMS45OC40WiIvPjwvZz48ZGVmcz48Y2xpcFBhdGggaWQ9ImEiPjxwYXRoIGZpbGw9Ii\
NmZmYiIGQ9Ik0wIDBoNDB2NDBIMHoiLz48L2NsaXBQYXRoPjwvZGVmcz48L3N2Zz4=`,
        }
      }
      case 'JSON':
      case 'Scatterplot': {
        return {
          axis: {
            x: { label: 'x-axis label', scale: 'linear' },
            y: { label: 'y-axis label', scale: 'logarithmic' },
          },
          focus: { x: 1.7, y: 2.1, zoom: 3.0 },
          points: { labels: 'visible' },
          data: [
            { x: 0.1, y: 0.7, label: 'foo', color: 'FF0000', shape: 'circle', size: 0.2 },
            { x: 0.4, y: 0.2, label: 'baz', color: '0000FF', shape: 'square', size: 0.3 },
          ],
        }
      }
      case 'Geo Map': {
        return {
          latitude: 37.8,
          longitude: -122.45,
          zoom: 15,
          controller: true,
          showingLabels: true, // Enables presenting labels when hovering over a point.
          layers: [
            {
              type: 'Scatterplot_Layer',
              data: [
                {
                  latitude: 37.8,
                  longitude: -122.45,
                  color: [255, 0, 0],
                  radius: 100,
                  label: 'an example label',
                },
              ],
            },
          ],
        }
      }
      default: {
        return {}
      }
    }
  }

  return { types, get, sampleData, clear }
})
