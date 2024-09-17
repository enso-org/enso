import testCases from '@/components/GraphEditor/__tests__/clipboardTestCases.json' assert { type: 'json' }
import {
  isSpreadsheetTsv,
  nodesFromClipboardContent,
  nodesToClipboardData,
  tsvTableToEnsoExpression,
} from '@/components/GraphEditor/clipboard'
import { type Node } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { nodeFromAst } from '@/util/ast/node'
import { Blob } from 'node:buffer'
import { expect, test } from 'vitest'
import { assertDefined } from 'ydoc-shared/util/assert'
import { type VisualizationMetadata } from 'ydoc-shared/yjsModel'

test.each([
  {
    description: 'Unpaired surrogate',
    tableData: '𝌆\t\uDAAA',
    expectedEnsoExpression: "'𝌆\\t\\u{daaa}'.to Table",
  },
  {
    description: 'Multiple rows, empty cells',
    tableData: [
      '\t36\t52',
      '11\t\t4.727272727',
      '12\t\t4.333333333',
      '13\t2.769230769\t4',
      '14\t2.571428571\t3.714285714',
      '15\t2.4\t3.466666667',
      '16\t2.25\t3.25',
      '17\t2.117647059\t3.058823529',
      '19\t1.894736842\t2.736842105',
      '21\t1.714285714\t2.476190476',
      '24\t1.5\t2.166666667',
      '27\t1.333333333\t1.925925926',
      '30\t1.2\t',
    ].join('\n'),
    expectedEnsoExpression:
      "'\\t36\\t52\\n11\\t\\t4.727272727\\n12\\t\\t4.333333333\\n13\\t2.769230769\\t4\\n14\\t2.571428571\\t3.714285714\\n15\\t2.4\\t3.466666667\\n16\\t2.25\\t3.25\\n17\\t2.117647059\\t3.058823529\\n19\\t1.894736842\\t2.736842105\\n21\\t1.714285714\\t2.476190476\\n24\\t1.5\\t2.166666667\\n27\\t1.333333333\\t1.925925926\\n30\\t1.2\\t'.to Table",
  },
])('Enso expression from Excel data: $description', ({ tableData, expectedEnsoExpression }) => {
  expect(tsvTableToEnsoExpression(tableData)).toEqual(expectedEnsoExpression)
})

class MockClipboardItem {
  readonly types: ReadonlyArray<string>

  constructor(private data: Record<string, Blob>) {
    this.types = Object.keys(data)
  }

  getType(type: string): Promise<Blob> {
    const blob = this.data[type]
    assertDefined(blob)
    return Promise.resolve(blob)
  }
}

const testNodeInputs: {
  code: string
  visualization?: VisualizationMetadata
  colorOverride?: string
}[] = [
  { code: '2 + 2' },
  { code: 'foo = bar' },
  { code: '## Documentation\n2 + 2', colorOverride: 'mauve' },
  { code: '## Documentation\nfoo = 2 + 2' },
]
const testNodes = testNodeInputs.map(({ code, visualization, colorOverride }) => {
  const root = Ast.Ast.parse(code)
  root.setNodeMetadata({ visualization, colorOverride })
  const node = nodeFromAst(root, false)
  assertDefined(node)
  // `nodesToClipboardData` only needs the `NodeDataFromAst` fields of `Node`, because it reads the metadata directly
  // from the AST.
  return node as Node
})
test.each([...testNodes.map((node) => [node]), testNodes])(
  'Copy and paste nodes',
  async (...sourceNodes) => {
    const clipboardItem = clipboardItemFromTypes(nodesToClipboardData(sourceNodes))
    const pastedNodes = await nodesFromClipboardContent([clipboardItem])
    sourceNodes.forEach((sourceNode, i) => {
      expect(pastedNodes[i]?.documentation).toBe(sourceNode.docs?.documentation())
      expect(pastedNodes[i]?.expression).toBe(sourceNode.innerExpr.code())
      expect(pastedNodes[i]?.metadata?.colorOverride).toBe(sourceNode.colorOverride)
      expect(pastedNodes[i]?.metadata?.visualization).toBe(sourceNode.vis)
    })
  },
)

function clipboardItemFromTypes(types: Record<string, string>): ClipboardItem {
  return new MockClipboardItem(
    Object.fromEntries(Object.entries(types).map(([key, value]) => [key, new Blob([value])])),
  ) as any
}

/* Creating a new test case:
 *
 * Obtaining `raw` clipboard HTML data from a spreadsheet:
 * - Copy a range of a spreadsheet in the source application
 * - In Chrome/Chromium, go to `https://evercoder.github.io/clipboard-inspector/`
 * - Paste into the page with the keyboard (not the button on the page)
 *   [the keyboard binding uses the legacy API, which Chromium doesn't sanitize]
 * - In the resulting type/getData(type) table, click "Copy as plain text" in the `text/html` row.
 *
 * Obtaining browser-sanitized HTML data from raw data:
 * 1. Load the raw data into the clipboard as 'text/html':
 *   - In Chromium, create a new tab and open the Developer Console
 *   - In the console, set `htmlData = <pasted raw HTML>`
 *   - Run: `setTimeout(async () => { await window.navigator.clipboard.write([new ClipboardItem({ 'text/html': new Blob([htmlData], { type: 'text/html' })})]); console.log('ok') }, 2000)`
 *   - After pressing Enter, quickly click the document background
 *   - Wait for 'ok' to be logged to the console
 * 2. In the target browser, go to `https://evercoder.github.io/clipboard-inspector/`
 *   - Click the "Paste using the Clipboard API" button
 *     [the button reads the clipboard via the async API with the default options, obtaining sanitized data]
 *   - Copy the `text/html` data
 */
type BrowserNameAndVersion = `${string}-${string}`
interface RecognitionCase {
  spreadsheet: string
  html: Record<BrowserNameAndVersion, string> & { raw: string }
}
interface FullStackCase extends RecognitionCase {
  plainText: string
  ensoCode: string
}
type SpreadsheetTestCase = RecognitionCase | FullStackCase
const spreadsheetTestCases: SpreadsheetTestCase[] = testCases.spreadsheetTestCases

test.each(spreadsheetTestCases)('Spreadsheet test case: $spreadsheet', async (testCase) => {
  for (const [version, htmlContent] of Object.entries(testCase.html)) {
    expect(isSpreadsheetTsv(htmlContent), `${version} version`).toBe(true)
    if ('plainText' in testCase) {
      const nodes = await nodesFromClipboardContent([
        clipboardItemFromTypes({
          'text/html': htmlContent,
          'text/plain': testCase.plainText,
        }),
      ])
      expect(nodes.length).toBe(1)
      assertDefined(nodes[0])
      expect(nodes[0].expression).toBe(testCase.ensoCode)
    }
  }
})
