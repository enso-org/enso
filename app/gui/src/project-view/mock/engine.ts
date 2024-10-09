import { Pattern } from '@/util/ast/match'
import type { MockYdocProviderImpl } from '@/util/crdt'
import type { WebSocketHandler } from '@/util/net'
import type { QualifiedName } from '@/util/qualifiedName'
import * as random from 'lib0/random'
import * as Ast from 'ydoc-shared/ast'
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
import type { MockTransportData } from 'ydoc-shared/util/net'
import { uuidToBits } from 'ydoc-shared/uuid'
import * as Y from 'yjs'
import { mockFsDirectoryHandle, type FileTree } from '../util/convert/fsAccess'
import { mockDataWSHandler as originalMockDataWSHandler } from './dataServer'
import mockDb from './mockSuggestions.json' assert { type: 'json' }

const mockProjectId = random.uuidv4() as Uuid
const standardBase = 'Standard.Base' as QualifiedName

function placeholderGroups(): LibraryComponentGroup[] {
  return [
    { color: '#4D9A29', name: 'Input', library: standardBase, exports: [] },
    { color: '#B37923', name: 'Web', library: standardBase, exports: [] },
    { color: '#9735B9', name: 'Parse', library: standardBase, exports: [] },
    { color: '#4D9A29', name: 'Select', library: standardBase, exports: [] },
    { color: '#B37923', name: 'Join', library: standardBase, exports: [] },
    { color: '#9735B9', name: 'Transform', library: standardBase, exports: [] },
    { color: '#4D9A29', name: 'Output', library: standardBase, exports: [] },
  ]
}

const mainFile = `\
## Module documentation
from Standard.Base import all

func1 arg =
    f2 = Main.func2 arg
    result = f2 - 5
    result

func2 a =
    r = 42 + a
    r

## The main method
main =
    five = 5
    ten = 10
    sum = five + ten
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
`

const fileTree = {
  src: {
    get 'Main.enso'() {
      return mainFile
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

const mockVizPreprocessors: Record<string, Uint8Array | ((params: string[]) => Uint8Array)> = {
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
  'Standard.Visualization.Widgets.get_widget_json': (params) => mockWidgetConfiguration(params[0]),

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
}

function mockWidgetConfiguration(method: string | undefined) {
  switch (method) {
    case '.read':
      return encodeJSON([
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
      ])
    case '.select_columns':
      return encodeJSON([
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
      ])
    case '.aggregate':
      return encodeJSON([
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
      ])
    default:
      return encodeJSON([])
  }
}

function createMessageId(builder: Builder) {
  const messageUuid = random.uuidv4()
  const [leastSigBits, mostSigBits] = uuidToBits(messageUuid)
  return EnsoUUID.createEnsoUUID(builder, leastSigBits, mostSigBits)
}

function createId(id: Uuid) {
  const [low, high] = uuidToBits(id)
  return (builder: Builder) => EnsoUUID.createEnsoUUID(builder, low, high)
}

function sendVizData(id: Uuid, config: VisualizationConfiguration, expressionId?: Uuid) {
  const vizDataHandler =
    typeof config.expression === 'string' ?
      // Getting widget configuration is a special case, where we sometimes pass lambda as
      // expression to discard the input value
      /^[a-z_]+ *->.*get_widget_json/.test(config.expression) ?
        mockWidgetConfiguration(config.positionalArgumentsExpressions?.at(0))
      : mockVizPreprocessors[`${config.visualizationModule}.${config.expression}`]
    : mockVizPreprocessors[`${config.expression.definedOnType}.${config.expression.name}`]
  if (!vizDataHandler || !sendData) return
  const vizData =
    vizDataHandler instanceof Uint8Array ? vizDataHandler : (
      vizDataHandler(config.positionalArgumentsExpressions ?? [])
    )
  const exprId = expressionId ?? visualizationExprIds.get(id)
  sendVizUpdate(id, config.executionContextId, exprId, vizData)
}

function sendVizUpdate(
  id: Uuid,
  executionCtxId: Uuid,
  exprId: Uuid | undefined,
  vizData: Uint8Array,
) {
  if (!sendData) return
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
  sendData(builder.finish(rootTable).toArrayBuffer())
}

let sendData: ((data: string | Blob | ArrayBufferLike | ArrayBufferView) => void) | undefined

export const mockLSHandler: MockTransportData = async (method, data, transport) => {
  switch (method) {
    case 'session/initProtocolConnection':
      return {
        contentRoots: [{ type: 'Project', id: mockProjectId }],
      } satisfies response.InitProtocolConnection
    case 'executionContext/create': {
      const data_ = data as {
        contextId: ContextId
      }
      setTimeout(
        () => transport.emit('executionContext/executionComplete', { contextId: data_.contextId }),
        100,
      )
      return {
        contextId: data_.contextId,
      }
    }
    case 'executionContext/attachVisualization': {
      const data_ = data as {
        visualizationId: Uuid
        expressionId: ExpressionId
        visualizationConfig: VisualizationConfiguration
      }
      visualizations.set(data_.visualizationId, data_.visualizationConfig)
      visualizationExprIds.set(data_.visualizationId, data_.expressionId)
      sendVizData(data_.visualizationId, data_.visualizationConfig)
      return
    }
    case 'executionContext/detachVisualization': {
      const data_ = data as {
        visualizationId: Uuid
        expressionId: ExpressionId
        contextId: ContextId
      }
      visualizations.delete(data_.visualizationId)
      visualizationExprIds.delete(data_.visualizationId)
      return
    }
    case 'executionContext/modifyVisualization': {
      const data_ = data as {
        visualizationId: Uuid
        visualizationConfig: VisualizationConfiguration
      }
      visualizations.set(data_.visualizationId, data_.visualizationConfig)
      sendVizData(data_.visualizationId, data_.visualizationConfig)
      return
    }
    case 'executionContext/executeExpression': {
      const data_ = data as {
        executionContextId: ContextId
        visualizationId: Uuid
        expressionId: ExpressionId
        expression: string
      }
      const aiPromptPat = Pattern.parse('Standard.Visualization.AI.build_ai_prompt __ . to_json')
      const exprAst = Ast.parse(data_.expression)
      if (aiPromptPat.test(exprAst)) {
        sendVizUpdate(
          data_.visualizationId,
          data_.executionContextId,
          data_.expressionId,
          encodeJSON('Could you __$$GOAL$$__, please?'),
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
        sendVizData(data_.visualizationId, visualizationConfig, data_.expressionId)
      }
      return
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
    case 'executionContext/push':
    case 'executionContext/pop':
    case 'executionContext/recompute':
    case 'executionContext/setExecutionEnvironment':
    case 'capability/acquire':
      return {}
    case 'file/list': {
      const data_ = data as { path: Path }
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
      const { prompt } = data
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

const directory = mockFsDirectoryHandle(fileTree, '(root)')

export const mockDataHandler: WebSocketHandler = originalMockDataWSHandler(
  async (segments) => {
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
  },
  (send) => (sendData = send),
)

export const mockYdocProvider: MockYdocProviderImpl = (msg, room, doc) => {
  setTimeout(() => {
    const srcFiles: Record<string, string> = fileTree.src
    if (room === 'index') {
      const modules = doc.getMap('modules')
      for (const file in srcFiles) modules.set(file, new Y.Doc({ guid: `mock-${file}` }))
    } else if (room.startsWith('mock-')) {
      const fileContents = srcFiles[room.slice('mock-'.length)]
      if (fileContents) new Ast.MutableModule(doc).syncToCode(fileContents)
    }
    msg.emit('sync', [])
  }, 0)
}
