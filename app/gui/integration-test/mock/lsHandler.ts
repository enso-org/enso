/// <reference types="wicg-file-system-access" />

import {
  GET_WIDGETS_METHOD,
  WIDGETS_ENSO_MODULE,
} from '@/components/GraphEditor/widgets/WidgetFunction/consts'
import * as Ast from '@/util/ast/abstract'
import type { QualifiedName } from '@/util/qualifiedName'
import { ErrorCode } from 'ydoc-shared/languageServer'
import type {
  ContextId,
  LibraryComponentGroup,
  Path,
  Uuid,
  response,
} from 'ydoc-shared/languageServerTypes'
import type { SuggestionEntry } from 'ydoc-shared/languageServerTypes/suggestions'
import {
  VIS_SLOT_FIELDS,
  Visualizations,
  type VisRequestPreprocessor,
} from 'ydoc-shared/visualizations'
import { VISUALIZATIONS_SUBDOC_KEY } from 'ydoc-shared/yjsModel'
import { Doc, Map as YMap } from 'yjs'
import mockDb from './data/mockSuggestions.json' with { type: 'json' }
import { mockDataWSHandler } from './dataServer'

const mockProjectId = crypto.randomUUID() as Uuid
const standardBase = 'Standard.Base' as QualifiedName

function placeholderGroups(): LibraryComponentGroup[] {
  return [
    { color: '#4D9A29', name: 'File', library: standardBase, exports: [] },
    { color: '#B37923', name: 'Web', library: standardBase, exports: [] },
    { color: '#9735B9', name: 'DateTime', library: standardBase, exports: [] },
    { color: '#4D9A29', name: 'Metadata', library: standardBase, exports: [] },
    { color: '#B37923', name: 'Constants', library: standardBase, exports: [] },
    { color: '#9735B9', name: 'Conversions', library: standardBase, exports: [] },
  ]
}

const mainFile = `\
## Module documentation
from Standard.Base import all

## A User Defined Function
func1 arg1 =
    f2 = Main.func2 arg
    result = f2 - 5
    result

func2 a =
    r = 42 + a
    r

## The main method

   Here we test images:

   ![Image](/images/image.png)
   ![Image](../images/image.png)
   ![Image](</images/image.png>)
   
   This is a video:

   ![Video](<https://www.youtube-nocookie.com/embed/dQw4w9WgXcQ>)

   Here is a link: <https://example.com>

   [A reference to another project](enso://Users/user%20name/MockProject)

   Nested lists:
    - List element
      - Nested list element
        - Very nested list element
    - Outer list element
main =
    five = 5
    ten = 10
    twenty = 20
    sum = five + ten + twenty
    prod = sum * 3
    ## This node can be entered
    final = Main.func1 prod
    list = []
    text = 'test'
    
    # Widget tests
    data = Data.read
    filtered = data.filter
    aggregated = data.aggregate
    autoscoped = data.aggregate [..Group_By]
    selected = data.select_columns
    table = data.set (expr "")

# To test for regressions in #12476, this line is really long and we test that the code editor doesn't resize to fit it. This line is really really long. This line is really REALLY long.
`

const fileTree = {
  src: {
    get 'Main.enso'() {
      return mainFile
    },
  },
  images: {
    get 'image.png'() {
      return new Uint16Array([
        20617, 18254, 2573, 2586, 0, 3328, 18505, 21060, 0, 768, 0, 768, 772, 0, 41984, 43014, 140,
        0, 20501, 21580, 65093, 13106, 11262, 64043, 27756, 24571, 64863, 14906, 12030, 65070,
        10023, 29424, 11222, 0, 4352, 17481, 21569, 55048, 28771, 24661, 4960, 24672, 52, 768, 161,
        21933, 29603, 124, 0, 18688, 20037, 44612, 24642, 130,
      ]).buffer
    },
  },
}

const encoder = new TextEncoder()
const encodeJSON = (data: unknown) => encoder.encode(JSON.stringify(data))

/** Stable key for the AI prompt builder, see {@link slotPreprocessorKey}. */
const AI_BUILD_PROMPT_KEY = '@@AI.build_ai_prompt'

const scatterplotJson = (params: string[]) =>
  encodeJSON({
    visualizedExpr: params[0],
    axis: {
      x: { label: 'x-axis label', scale: 'linear' },
      y: { label: 'y-axis label', scale: 'logarithmic' },
    },
    points: { labels: 'visible' },
    data: [
      { x: 0.1, y: 0.7, label: 'foo', color: '#FF0000', shape: 'circle', size: 0.2 },
      { x: 0.4, y: 0.2, label: 'baz', color: '#0000FF', shape: 'square', size: 0.3 },
    ],
  })

const mockVizPreprocessors: Record<string, Uint8Array | ((params: string[]) => Uint8Array | null)> =
  {
    // JSON
    'Standard.Visualization.Preprocessor.default_preprocessor': scatterplotJson,
    'Standard.Visualization.Scatter_Plot.process_to_json_text': scatterplotJson,
    'Standard.Visualization.SQL.Visualization.prepare_visualization': encodeJSON({
      dialect: 'sql',
      code: `SELECT * FROM \`foo\` WHERE \`a\` = ? AND b LIKE ?;`,
      interpolations: [
        // eslint-disable-next-line camelcase
        { enso_type: 'Data.Numbers.Number', value: '123' },
        // eslint-disable-next-line camelcase
        { enso_type: 'Builtins.Main.Text', value: "a'bcd" },
      ],
    }),
    'Standard.Visualization.Geo_Map.process_to_json_text': encodeJSON({
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
    }),
    'Standard.Visualization.Histogram.process_to_json_text': encodeJSON({
      axis: {
        x: { label: 'x-axis label', scale: 'linear' },
        y: { label: 'y-axis label', scale: 'logarithmic' },
      },
      color: 'rgb(1.0,0.0,0.0)',
      bins: 10,
      data: {
        values: [0.1, 0.2, 0.1, 0.15, 0.7],
      },
    }),
    'Standard.Visualization.Table.Visualization.prepare_visualization': encodeJSON({
      type: 'Matrix',
      // eslint-disable-next-line camelcase
      column_count: 5,
      // eslint-disable-next-line camelcase
      all_rows_count: 10,
      json: Array.from({ length: 10 }, (_, i) => Array.from({ length: 5 }, (_, j) => `${i},${j}`)),
    }),
    'Standard.Visualization.Warnings.process_to_json_text': encodeJSON([
      'warning 1',
      "warning 2!!&<>;'\x22",
    ]),
    'Standard.Visualization.Widgets.get_widget_json': (params) =>
      mockWidgetConfiguration(params[0]),

    // The following visualizations do not have unique transformation methods, and as such are only kept
    // for posterity.
    Image: encodeJSON({
      mediaType: 'image/svg+xml',
      base64: `PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI0MCIgaGVpZ2h0PSI0\
MCI+PGcgY2xpcC1wYXRoPSJ1cmwoI2EpIj48cGF0aCBkPSJNMjAuMDUgMEEyMCAyMCAwIDAgMCAwIDIwLjA1IDIwLjA2IDIwLjA\
2IDAgMSAwIDIwLjA1IDBabTAgMzYuMDVjLTguOTMgMC0xNi4xLTcuMTctMTYuMS0xNi4xIDAtOC45NCA3LjE3LTE2LjEgMTYuMS\
0xNi4xIDguOTQgMCAxNi4xIDcuMTYgMTYuMSAxNi4xYTE2LjE4IDE2LjE4IDAgMCAxLTE2LjEgMTYuMVoiLz48cGF0aCBkPSJNM\
jcuMTIgMTcuNzdhNC42OCA0LjY4IDAgMCAxIDIuMzkgNS45MiAxMC4yMiAxMC4yMiAwIDAgMS05LjU2IDYuODZBMTAuMiAxMC4y\
IDAgMCAxIDkuNzcgMjAuMzZzMS41NSAyLjA4IDQuNTcgMi4wOGMzLjAxIDAgNC4zNi0xLjE0IDUuNi0yLjA4IDEuMjUtLjkzIDI\
uMDktMyA1LjItMyAuNzMgMCAxLjQ2LjIgMS45OC40WiIvPjwvZz48ZGVmcz48Y2xpcFBhdGggaWQ9ImEiPjxwYXRoIGZpbGw9Ii\
NmZmYiIGQ9Ik0wIDBoNDB2NDBIMHoiLz48L2NsaXBQYXRoPjwvZGVmcz48L3N2Zz4=`,
    }),
    Heatmap: encodeJSON([
      ['A', 'B', 'C', 'D', 'A'],
      ['D', 'E', 'D', 'X', 'Z'],
      [50, 25, 40, 20, 10],
    ]),
    'Standard.Visualization.Widgets.column_names_json': encodeJSON(['Column A', 'Column B']),
    // AI prompt template returned in place of evaluating
    // `Standard.Visualization.AI.build_ai_prompt`. The client substitutes the
    // user's goal for `__$$GOAL$$__` and forwards the result to `ai/completion`.
    [AI_BUILD_PROMPT_KEY]: encodeJSON('Could you __$$GOAL$$__, please?'),
  }

const initialMockWidgetConfigurations: Map<string, Uint8Array> = new Map([
  [
    '.read',
    encodeJSON([
      [
        'path',
        {
          type: 'Widget',
          constructor: 'Single_Choice',
          label: null,
          values: [
            {
              type: 'Choice',
              constructor: 'Option',
              value: '"File 1"',
              label: 'File 1',
              parameters: [],
            },
            {
              type: 'Choice',
              constructor: 'Option',
              value: '"File 2"',
              label: 'File 2',
              parameters: [],
            },
          ],
          display: { type: 'Display', constructor: 'Always' },
        },
      ],
    ]),
  ],
  [
    '.select_columns',
    encodeJSON([
      [
        'columns',
        {
          type: 'Widget',
          constructor: 'Multiple_Choice',
          label: null,
          values: [
            {
              type: 'Choice',
              constructor: 'Option',
              value: "'Column A'",
              label: 'Column A',
              parameters: [],
            },
            {
              type: 'Choice',
              constructor: 'Option',
              value: "'Column B'",
              label: 'Column B',
              parameters: [],
            },
          ],
          display: { type: 'Display', constructor: 'Always' },
        },
      ],
    ]),
  ],
  [
    '.aggregate',
    encodeJSON([
      [
        'columns',
        {
          type: 'Widget',
          constructor: 'Vector_Editor',
          /* eslint-disable camelcase */
          item_default: 'Aggregate_Column.Group_By',
          item_editor: {
            type: 'Widget',
            constructor: 'Single_Choice',
            label: null,
            values: [
              {
                type: 'Choice',
                constructor: 'Option',
                value: 'Standard.Table.Aggregate_Column.Aggregate_Column.Group_By',
                label: null,
                parameters: [
                  [
                    'column',
                    {
                      type: 'Widget',
                      constructor: 'Single_Choice',
                      label: null,
                      values: [
                        {
                          type: 'Choice',
                          constructor: 'Option',
                          value: '"column 1"',
                          label: 'column 1',
                          parameters: [],
                        },
                        {
                          type: 'Choice',
                          constructor: 'Option',
                          value: '"column 2"',
                          label: 'column 2',
                          parameters: [],
                        },
                      ],
                      display: { type: 'Display', constructor: 'Always' },
                    },
                  ],
                ],
              },
              {
                type: 'Choice',
                constructor: 'Option',
                value: 'Standard.Table.Aggregate_Column.Aggregate_Column.Count',
                label: null,
                parameters: [],
              },
              {
                type: 'Choice',
                constructor: 'Option',
                value: 'Standard.Table.Aggregate_Column.Aggregate_Column.Count_Distinct',
                label: null,
                parameters: [
                  [
                    'columns',
                    {
                      type: 'Widget',
                      constructor: 'Single_Choice',
                      label: null,
                      values: [
                        {
                          type: 'Choice',
                          constructor: 'Option',
                          value: '"column 1"',
                          label: 'column 1',
                          parameters: [],
                        },
                        {
                          type: 'Choice',
                          constructor: 'Option',
                          value: '"column 2"',
                          label: 'column 2',
                          parameters: [],
                        },
                      ],
                      display: { type: 'Display', constructor: 'Always' },
                    },
                  ],
                ],
              },
            ],
            display: { type: 'Display', constructor: 'Always' },
          },
          /* eslint-enable camelcase */
          display: { type: 'Display', constructor: 'Always' },
        },
      ],
    ]),
  ],
  [
    '.expr',
    encodeJSON([
      [
        'expression',
        {
          type: 'Widget',
          constructor: 'Text_Input',
          syntax: 'enso-table-expression',
          display: { type: 'Display', constructor: 'When_Modified' },
        },
      ],
    ]),
  ],
])

let mockWidgetConfigurations: Map<string, Uint8Array> = new Map(initialMockWidgetConfigurations)

/** Clear standard widget configurations. Use `updateMockWidgetConfiguration` to set a specific configuration needed for test. */
export function clearMockWidgetConfigurations() {
  mockWidgetConfigurations.clear()
  preprocessorOverrides.clear()
}

/** Restore standard mocks of widget configurations. */
export function restoreMockWidgetConfigurations() {
  mockWidgetConfigurations = new Map(initialMockWidgetConfigurations)
  preprocessorOverrides.clear()
}

/**
 * Per-test reset for {@link updateVisualizationData} overrides. Does not
 * touch {@link mockWidgetConfigurations}: some test describes own the widget
 * map via their own `beforeEach`/`afterEach` hooks and must not be stomped on.
 */
export function clearPreprocessorOverrides() {
  preprocessorOverrides.clear()
}

function mockWidgetConfiguration(method: string | undefined) {
  if (!method) return null
  return mockWidgetConfigurations.get(method) ?? null
}

export const mockLSHandler = async (
  method: string,
  params: object,
  sendMessage: (message: { method: string; params: object }) => void,
  _sendBinary: (data?: ArrayBuffer) => void,
) => {
  switch (method) {
    case 'session/initProtocolConnection':
      return {
        contentRoots: [{ type: 'Project', id: mockProjectId }],
      } satisfies response.InitProtocolConnection
    case 'executionContext/create': {
      const data_ = params as {
        contextId: ContextId
      }
      setTimeout(
        () =>
          sendMessage({
            method: 'executionContext/executionComplete',
            params: { contextId: data_.contextId },
          }),
        100,
      )
      return { contextId: data_.contextId }
    }
    case 'executionContext/push':
    case 'executionContext/pop':
    case 'executionContext/recompute':
    case 'executionContext/setExecutionEnvironment': {
      return {}
    }
    case 'search/getSuggestionsDatabase':
      return {
        entries: mockDb.map((suggestion, id) => ({
          id,
          suggestion: suggestion as SuggestionEntry,
        })),
        currentVersion: 1,
      } satisfies response.GetSuggestionsDatabase
    case 'runtime/getComponentGroups':
      return { componentGroups: placeholderGroups() } satisfies response.GetComponentGroups
    case 'capability/acquire':
      return {}
    case 'file/list': {
      const data_ = params as { path: Path }
      if (!data_.path) return Promise.reject(`'path' parameter missing in '${method}'`)
      if (data_.path.rootId !== mockProjectId)
        return Promise.reject(
          `Only the project's 'rootId' is supported, got '${data_.path.rootId}'`,
        )
      let child: FileTree | string | ArrayBuffer | undefined = fileTree
      if (child) {
        for (const segment of data_.path.segments) {
          child = child?.[segment]
          if (!child || typeof child === 'string' || child instanceof ArrayBuffer) break
        }
      }
      if (!child)
        return Promise.reject({
          code: ErrorCode.FILE_NOT_FOUND,
          message: `Folder '/${data_.path.segments.join('/')}' not found.`,
        })
      if (typeof child === 'string' || child instanceof ArrayBuffer)
        return Promise.reject({
          code: ErrorCode.NOT_DIRECTORY,
          message: `File '/${data_.path.segments.join('/')}' is not a folder.`,
        })
      return {
        paths: Object.entries(child).map(([name, entry]) => ({
          type: typeof entry === 'string' || entry instanceof ArrayBuffer ? 'File' : 'Directory',
          name,
          path: { rootId: data_.path.rootId, segments: [...data_.path.segments, name] },
        })),
      } satisfies response.FileList
    }
    default:
      return Promise.reject(`Method '${method}' not mocked`)
  }
}

/**
 * Test-only overrides for mock preprocessor responses. A test pushes updated
 * data via {@link updateVisualizationData}; on receipt the mock ydoc provider
 * rewrites every matching ready/pending slot in the vis subdoc so the client
 * observes the new bytes through its normal reactive path.
 */
const preprocessorOverrides = new Map<string, Uint8Array>()

/**
 * Extract the preprocessor identifier from a slot's immutable request, matching
 * the scheme the pre-refactor mock used for the binary `VisualizationUpdate`
 * path. The client serializes preprocessor module + expression + positional
 * args into the slot's `request` field; we recover the same composite key so
 * the mock can route responses by preprocessor name.
 */
function slotPreprocessorKey(request: VisRequestPreprocessor): string | null {
  const expression = request.expression
  if (typeof expression === 'string') {
    if (/^[a-z_]+ *->.*get_widget_json/.test(expression)) {
      return request.positionalArgumentsExpressions?.at(0) ?? null
    }
    return `${request.visualizationModule}.${expression}`
  }
  if ('inFrame' in expression) {
    // The component browser's AI prompt path issues a one-shot
    // `executeExpression` request for `... . to_json` on the source node.
    // The leftmost identifier is the source node name (varies per test), so
    // route every AI-prompt request to a stable key.
    if (/Standard\.Visualization\.AI\.build_ai_prompt\b.*\.\s*to_json/.test(expression.inFrame)) {
      return AI_BUILD_PROMPT_KEY
    }
    const exprAst = Ast.parseExpression(expression.inFrame)
    if (!exprAst) return null
    const { func } = Ast.analyzeAppLike(exprAst)
    if (!(func instanceof Ast.PropertyAccess && func.lhs)) return null
    return `${func.lhs.code()}.${func.rhs.code()}`
  }
  if (expression.module === WIDGETS_ENSO_MODULE && expression.name === GET_WIDGETS_METHOD) {
    return request.positionalArgumentsExpressions?.at(0) ?? null
  }
  return `${expression.definedOnType}.${expression.name}`
}

function slotPositionalArgs(request: VisRequestPreprocessor): string[] {
  if (typeof request.expression === 'object' && 'inFrame' in request.expression) {
    const exprAst = Ast.parseExpression(request.expression.inFrame)
    if (!exprAst) return []
    const { args } = Ast.analyzeAppLike(exprAst)
    return args.map((ast) => ast.code())
  }
  return request.positionalArgumentsExpressions ?? []
}

/**
 * Look up mock response bytes for a given preprocessor key. The preprocessor
 * space is unified across both visualization queries
 * (`Module.expression`-keyed) and widget queries (keyed by the first
 * positional arg, e.g. `.read`), so tests can push responses for either via a
 * single {@link updateVisualizationData} call. Lookup order:
 *
 * 1. Test overrides set through {@link updateVisualizationData}.
 * 2. The widget configuration map - widget queries route here because the
 *    pre-refactor binary path called `mockWidgetConfiguration(positionalArgs[0])`.
 * 3. Built-in visualization mocks in {@link mockVizPreprocessors}.
 */
function responseBytesFor(
  preprocessorKey: string,
  positionalArgs: readonly string[],
): Uint8Array | null {
  const override = preprocessorOverrides.get(preprocessorKey)
  if (override) return override
  const widget = mockWidgetConfigurations.get(preprocessorKey)
  if (widget) return widget
  const mock = mockVizPreprocessors[preprocessorKey]
  if (mock instanceof Uint8Array) return mock
  if (typeof mock === 'function') return mock(Array.from(positionalArgs))
  return null
}

/**
 * All vis subdoc `slots` maps that the mock currently observes. One entry per
 * live WebSocket connection to the vis subdoc; iterated by
 * {@link updateVisualizationData} so a test's push reaches every connection.
 */
const visSlotsMaps = new Set<YMap<YMap<unknown>>>()

/** Write response bytes + `status: 'ready'` into `slot`. */
function writeSlotResponse(slot: YMap<unknown>, bytes: Uint8Array): void {
  const doc = slot.doc
  if (!doc) return
  doc.transact(() => {
    slot.set(VIS_SLOT_FIELDS.response, bytes)
    slot.set(VIS_SLOT_FIELDS.status, 'ready')
  })
}

/** If the slot's preprocessor is mocked, write the mock bytes into it. */
function maybeRespondToSlot(slot: YMap<unknown>): void {
  const request = slot.get(VIS_SLOT_FIELDS.request) as VisRequestPreprocessor | undefined
  if (!request) return
  const key = slotPreprocessorKey(request)
  if (!key) return
  const bytes = responseBytesFor(key, slotPositionalArgs(request))
  if (bytes) writeSlotResponse(slot, bytes)
}

/**
 * Test entry point: push `data` as the mock response for every slot whose
 * preprocessor matches `preprocessor`. Also caches the bytes for future slots
 * created after this call, and for widget lookups that hit
 * {@link mockWidgetConfigurations}.
 */
export function updateVisualizationData(preprocessor: string, data: unknown): void {
  const bytes = encodeJSON(data)
  preprocessorOverrides.set(preprocessor, bytes)
  mockWidgetConfigurations.set(preprocessor, bytes)
  for (const slots of visSlotsMaps) {
    for (const [, slot] of slots.entries()) {
      const request = slot.get(VIS_SLOT_FIELDS.request) as VisRequestPreprocessor | undefined
      if (!request) continue
      if (slotPreprocessorKey(request) === preprocessor) writeSlotResponse(slot, bytes)
    }
  }
}

/**
 * Stable guid for the mock vis subdoc, so the provider can recognize the
 * subdoc room when the client opens a WebSocket for it.
 */
const MOCK_VIS_SUBDOC_GUID = 'mock-visualizations-subdoc'

const directory = mockFsDirectoryHandle(fileTree, '(root)')

export const mockDataHandler = mockDataWSHandler(async (segments) => {
  if (!segments.length) return
  let file
  try {
    let dir = directory
    for (const segment of segments.slice(0, -1)) {
      dir = await dir.getDirectoryHandle(segment)
    }
    const fileHandle = await dir.getFileHandle(segments.at(-1)!)
    file = await fileHandle.getFile()
  } catch {
    return
  }
  return await file?.arrayBuffer()
})

export const mockYdocProvider = (room: string, doc: Doc) => {
  const srcFiles: Record<string, string> = fileTree.src
  if (room === 'index') {
    const modules = doc.getMap('modules')
    for (const file in srcFiles) modules.set(file, new Doc({ guid: `mock-${file}` }))
    // Install a vis subdoc placeholder so the client resolves the
    // visualizations container to a loadable subdoc and opens the matching
    // WebSocket room. The guid is fixed so the provider can recognize the
    // vis room when that second connection arrives.
    const visContainer = doc.getMap<Doc>('visualizations')
    visContainer.set(VISUALIZATIONS_SUBDOC_KEY, new Doc({ guid: MOCK_VIS_SUBDOC_GUID }))
  } else if (room === MOCK_VIS_SUBDOC_GUID) {
    const vis = new Visualizations(doc)
    // Retained for the lifetime of the mock process. Playwright page teardown
    // destroys the page-level WebSockets anyway, and iterating a stale map is
    // harmless - its slots resolve to `.doc === null` and the write is a
    // no-op.
    visSlotsMaps.add(vis.slots)
    vis.slots.observeDeep(() => {
      for (const [, slot] of vis.slots.entries()) {
        const status = slot.get(VIS_SLOT_FIELDS.status)
        if (status !== 'pending') continue
        maybeRespondToSlot(slot)
      }
    })
  } else if (room.startsWith('mock-')) {
    const fileContents = srcFiles[room.slice('mock-'.length)]
    if (fileContents) new Ast.MutableModule(doc).syncToCode(fileContents)
  }
}

/// <reference types="wicg-file-system-access" />
export interface FileTree {
  [name: string]: FileTree | string | ArrayBuffer
}

function arrayIsSame(a: unknown[], b: unknown) {
  return Array.isArray(b) && a.length === b.length && a.every((item, i) => b[i] === item)
}

/** TODO: add docs here. */
export function mockFsFileHandle(
  contents: string | ArrayBuffer,
  name: string,
  path: string[] = [],
): FileSystemFileHandle {
  return {
    kind: 'file',
    isFile: true,
    isDirectory: false,
    // Spreaded to avoid excess property error.
    ...{ _path: path },
    name,
    queryPermission() {
      // Unimplemented.
      throw new Error('Cannot query permission in a read-only mock.')
    },
    requestPermission() {
      // Unimplemented.
      throw new Error('Cannot request permission in a read-only mock.')
    },
    async isSameEntry(other) {
      return this.kind === other.kind && '_path' in other && arrayIsSame(path, other._path)
    },
    async getFile() {
      return new File([contents], name)
    },
    createWritable() {
      throw new Error('Cannot create a writable strean from a read-only mock.')
    },
  }
}

/** TODO: Add docs */
export function mockFsDirectoryHandle(
  tree: FileTree,
  name: string,
  path: string[] = [],
): FileSystemDirectoryHandle {
  return {
    kind: 'directory',
    isFile: false,
    isDirectory: true,
    name,
    // Spreaded to avoid excess property error.
    ...{ _path: path },
    async isSameEntry(other) {
      return this.kind === other.kind && '_path' in other && arrayIsSame(path, other._path)
    },
    async resolve(possibleDescendant) {
      if (!('_path' in possibleDescendant)) return null
      if (!Array.isArray(possibleDescendant._path)) return null
      if (possibleDescendant._path.length < path.length) return null
      if (possibleDescendant._path.slice(0, path.length).some((segment, i) => segment !== path[i]))
        return null
      const descendantPath: string[] = possibleDescendant._path
      return descendantPath.slice(path.length)
    },
    queryPermission() {
      // Unimplemented.
      throw new Error('Cannot query permission in a read-only mock.')
    },
    requestPermission() {
      // Unimplemented.
      throw new Error('Cannot request permission in a read-only mock.')
    },
    async getDirectoryHandle(name) {
      const entry = tree[name]
      if (!entry || typeof entry === 'string' || entry instanceof ArrayBuffer) {
        const error = new DOMException(
          `The directory '${[...path, name].join('/')}' was not found.`,
          'NotFoundError',
        )
        throw error
      }
      return mockFsDirectoryHandle(entry, name, [...path, name])
    },
    async getFileHandle(name) {
      const entry = tree[name]
      if (entry == null || (typeof entry !== 'string' && !(entry instanceof ArrayBuffer))) {
        const error = new DOMException(
          `The file '${[...path, name].join('/')}' could not be found.`,
          'NotFoundError',
        )
        throw error
      }
      return mockFsFileHandle(entry, name, [...path, name])
    },
    getDirectory(name) {
      return this.getDirectoryHandle(name)
    },
    getFile(name) {
      return this.getFileHandle(name)
    },
    async removeEntry() {
      throw new Error('Cannot remove an entry from a read-only mock.')
    },
    async *keys() {
      for (const name in tree) yield name
    },
    async *values() {
      for (const name in tree) {
        const entry = tree[name]!
        if (typeof entry === 'string' || entry instanceof ArrayBuffer) {
          yield mockFsFileHandle(entry, name, [...path, name])
        } else {
          yield mockFsDirectoryHandle(entry, name, [...path, name])
        }
      }
    },
    getEntries() {
      return this.values()
    },
    async *entries() {
      for (const name in tree) {
        const entry = tree[name]!
        if (typeof entry === 'string' || entry instanceof ArrayBuffer) {
          yield [name, mockFsFileHandle(entry, name, [...path, name])]
        } else {
          yield [name, mockFsDirectoryHandle(entry, name, [...path, name])]
        }
      }
    },
    [Symbol.asyncIterator]() {
      return this.entries()
    },
  }
}
