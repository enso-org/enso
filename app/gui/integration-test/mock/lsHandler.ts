/// <reference types="wicg-file-system-access" />

import {
  GET_WIDGETS_METHOD,
  WIDGETS_ENSO_MODULE,
} from '@/components/GraphEditor/widgets/WidgetFunction/consts'
import * as Ast from '@/util/ast/abstract'
import { Pattern } from '@/util/ast/match'
import type { QualifiedName } from '@/util/qualifiedName'
import {
  Builder,
  EnsoUUID,
  OutboundMessage,
  OutboundPayload,
  VisualizationContext,
  VisualizationUpdate,
} from 'ydoc-shared/binaryProtocol'
import { ErrorCode } from 'ydoc-shared/languageServer'
import type {
  ContextId,
  ExpressionId,
  LibraryComponentGroup,
  Path,
  Uuid,
  VisualizationConfiguration,
  response,
} from 'ydoc-shared/languageServerTypes'
import type { SuggestionEntry } from 'ydoc-shared/languageServerTypes/suggestions'
import { uuidToBits } from 'ydoc-shared/uuid'
import { Doc } from 'yjs'
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

const visualizations = new Map<Uuid, VisualizationConfiguration>()
const visualizationExprIds = new Map<Uuid, ExpressionId>()

const encoder = new TextEncoder()
const encodeJSON = (data: unknown) => encoder.encode(JSON.stringify(data))

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
}

/** Restore standard mocks of widget configurations. */
export function restoreMockWidgetConfigurations() {
  mockWidgetConfigurations = new Map(initialMockWidgetConfigurations)
}

function mockWidgetConfiguration(method: string | undefined) {
  if (!method) return null
  return mockWidgetConfigurations.get(method) ?? null
}

function createMessageId(builder: Builder) {
  const messageUuid = crypto.randomUUID()
  const [leastSigBits, mostSigBits] = uuidToBits(messageUuid)
  return EnsoUUID.createEnsoUUID(builder, leastSigBits, mostSigBits)
}

function createId(id: Uuid) {
  const [low, high] = uuidToBits(id)
  return (builder: Builder) => EnsoUUID.createEnsoUUID(builder, low, high)
}

type VizRequest = { type: 'widget'; id: string | undefined } | { type: 'visualization'; id: string }
function recognizeVizRequest(config: VisualizationConfiguration): VizRequest {
  if (typeof config.expression === 'string') {
    // Getting widget configuration is a special case, where we sometimes pass lambda as
    // expression to discard the input value
    if (/^[a-z_]+ *->.*get_widget_json/.test(config.expression)) {
      return { type: 'widget', id: config.positionalArgumentsExpressions?.at(0) } as VizRequest
    } else {
      return {
        type: 'visualization',
        id: `${config.visualizationModule}.${config.expression}`,
      } as VizRequest
    }
  } else if (
    config.expression.module === WIDGETS_ENSO_MODULE &&
    config.expression.name === GET_WIDGETS_METHOD
  ) {
    return { type: 'widget', id: config.positionalArgumentsExpressions?.at(0) } as VizRequest
  } else {
    return {
      type: 'visualization',
      id: `${config.expression.definedOnType}.${config.expression.name}`,
    } as VizRequest
  }
}

function makeVizData(id: Uuid, config: VisualizationConfiguration, expressionId?: Uuid) {
  const req = recognizeVizRequest(config)
  const vizDataHandler =
    req.type === 'visualization' ? mockVizPreprocessors[req.id] : mockWidgetConfiguration(req.id)
  if (!vizDataHandler) return
  const vizData =
    vizDataHandler instanceof Uint8Array ? vizDataHandler : (
      vizDataHandler(config.positionalArgumentsExpressions ?? [])
    )
  if (!vizData) return
  const exprId = expressionId ?? visualizationExprIds.get(id)
  return makeVizUpdate(id, config.executionContextId, exprId, vizData)
}

function makeVizUpdate(
  id: Uuid,
  executionCtxId: Uuid,
  exprId: Uuid | undefined,
  vizData: Uint8Array,
) {
  const builder = new Builder()
  const visualizationContextOffset = VisualizationContext.createVisualizationContext(
    builder,
    createId(id),
    createId(executionCtxId),
    exprId ? createId(exprId) : null,
  )
  const dataOffset = VisualizationUpdate.createDataVector(builder, vizData)
  const payload = VisualizationUpdate.createVisualizationUpdate(
    builder,
    visualizationContextOffset,
    dataOffset,
  )
  const rootTable = OutboundMessage.createOutboundMessage(
    builder,
    createMessageId,
    null, // correlationId
    OutboundPayload.VISUALIZATION_UPDATE,
    payload,
  )
  return builder.finish(rootTable).toArrayBuffer()
}

export const mockLSHandler = async (
  method: string,
  params: object,
  sendMessage: (message: { method: string; params: object }) => void,
  sendBinary: (data?: ArrayBuffer) => void,
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
    case 'executionContext/attachVisualization': {
      const data_ = params as {
        visualizationId: Uuid
        expressionId: ExpressionId
        visualizationConfig: VisualizationConfiguration
      }
      visualizations.set(data_.visualizationId, data_.visualizationConfig)
      visualizationExprIds.set(data_.visualizationId, data_.expressionId)
      sendBinary(makeVizData(data_.visualizationId, data_.visualizationConfig))
      return
    }
    case 'executionContext/detachVisualization': {
      const data_ = params as {
        visualizationId: Uuid
        expressionId: ExpressionId
        contextId: ContextId
      }
      visualizations.delete(data_.visualizationId)
      visualizationExprIds.delete(data_.visualizationId)
      return
    }
    case 'executionContext/modifyVisualization': {
      const data_ = params as {
        visualizationId: Uuid
        visualizationConfig: VisualizationConfiguration
      }
      visualizations.set(data_.visualizationId, data_.visualizationConfig)
      sendBinary(makeVizData(data_.visualizationId, data_.visualizationConfig))
      return
    }
    case 'executionContext/executeExpression': {
      const data_ = params as {
        executionContextId: ContextId
        visualizationId: Uuid
        expressionId: ExpressionId
        expression: string
      }
      const aiPromptPat = Pattern.parseExpression(
        'Standard.Visualization.AI.build_ai_prompt __ . to_json',
      )
      const exprAst = Ast.parseExpression(data_.expression)!
      if (aiPromptPat.test(exprAst)) {
        sendBinary(
          makeVizUpdate(
            data_.visualizationId,
            data_.executionContextId,
            data_.expressionId,
            encodeJSON('Could you __$$GOAL$$__, please?'),
          ),
        )
      } else {
        // Check if there's existing preprocessor mock which matches our expression
        const { func, args } = Ast.analyzeAppLike(exprAst)
        if (!(func instanceof Ast.PropertyAccess && func.lhs)) return
        const visualizationConfig: VisualizationConfiguration = {
          executionContextId: data_.executionContextId,
          visualizationModule: func.lhs.code(),
          expression: func.rhs.code(),
          positionalArgumentsExpressions: args.map((ast) => ast.code()),
        }
        sendBinary(makeVizData(data_.visualizationId, visualizationConfig, data_.expressionId))
      }
      return
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
    case 'ai/completion': {
      const { prompt } = params as { prompt: string }
      const match = /^Could you (.*), please\?$/.exec(prompt)
      if (!match) {
        return { code: 'How rude!' }
      } else if (match[1] === 'convert to table') {
        return { code: 'to_table' }
      } else {
        return { code: '"I don\'t understand, sorry"' }
      }
    }
    default:
      return Promise.reject(`Method '${method}' not mocked`)
  }
}

/** Prepare visualization update data sent by a mock binary endpoint */
export function makeVisUpdates(preprocessor: string, data: unknown) {
  const updates: ArrayBuffer[] = []
  for (const [id, config] of visualizations.entries()) {
    if (recognizeVizRequest(config).id === preprocessor) {
      const exprId = visualizationExprIds.get(id)
      const vizData = encodeJSON(data)
      updates.push(makeVizUpdate(id, config.executionContextId, exprId, vizData))
      mockWidgetConfigurations.set(preprocessor, vizData)
    }
  }
  return updates
}

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
