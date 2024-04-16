import {
  excelTableToEnso,
  nodesFromClipboardContent,
  nodesToClipboardData,
} from '@/components/GraphEditor/clipboard'
import { Ast } from '@/util/ast'
import { initializePrefixes, nodeFromAst } from '@/util/ast/node'
import { Vec2 } from '@/util/data/vec2'
import { Blob } from 'node:buffer'
import { initializeFFI } from 'shared/ast/ffi'
import { assertDefined } from 'shared/util/assert'
import { type VisualizationMetadata } from 'shared/yjsModel'
import { expect, test } from 'vitest'

await initializeFFI()
initializePrefixes()

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
  expect(excelTableToEnso(tableData)).toEqual(expectedEnsoExpression)
})

class MockClipboardItem {
  readonly types: ReadonlyArray<string>

  constructor(private data: Record<string, Blob>) {
    this.types = Object.keys(data)
  }

  getType(type: string): Blob {
    const blob = this.data[type]
    assertDefined(blob)
    return blob
  }
}

const testNodeInputs: { code: string; vis?: VisualizationMetadata; colorOverride?: string }[] = [
  { code: '2 + 2' },
  { code: 'foo = bar' },
  { code: '## Documentation\n2 + 2', colorOverride: 'mauve' },
  { code: '## Documentation\nfoo = 2 + 2' },
]
const testNodes = testNodeInputs.map(({ code, vis, colorOverride }) => {
  const node = nodeFromAst(Ast.Ast.parse(code))
  assertDefined(node)
  return {
    ...node,
    vis: vis ?? null,
    colorOverride: colorOverride ?? null,
    // Not copied; included for type completeness:
    zIndex: 0,
    position: Vec2.Zero,
  }
})
test.each([...testNodes.map((node) => [node]), testNodes])(
  'Copy and paste nodes',
  async (...sourceNodes) => {
    const clipboardItems = nodesToClipboardData(
      sourceNodes,
      (data) => new MockClipboardItem(data as any) as any,
      (parts, type) => new Blob(parts, { type }) as any,
    )
    const pastedNodes = await nodesFromClipboardContent(clipboardItems)
    sourceNodes.forEach((sourceNode, i) => {
      expect(pastedNodes[i]?.documentation).toEqual(sourceNode.documentation)
      expect(pastedNodes[i]?.expression).toEqual(sourceNode.innerExpr.code())
      expect(pastedNodes[i]?.colorOverride ?? null).toEqual(sourceNode.colorOverride)
      expect(pastedNodes[i]?.visualization ?? null).toEqual(sourceNode.vis)
    })
  },
)
