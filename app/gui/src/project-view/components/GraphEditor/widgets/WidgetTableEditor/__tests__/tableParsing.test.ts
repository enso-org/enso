import { DEFAULT_COLUMN_PREFIX } from '@/components/GraphEditor/widgets/WidgetTableEditor/tableInputArgument'
import {
  parseTsvData,
  tableToEnsoExpression,
} from '@/components/GraphEditor/widgets/WidgetTableEditor/tableParsing'
import { assert } from '@/util/assert'
import { expect, test } from 'vitest'

test.each([
  {
    description: 'Unpaired surrogate',
    tableData: '𝌆\t\uDAAA',
    expectedEnsoExpression: `Table.input [['${DEFAULT_COLUMN_PREFIX}1', ['𝌆']], ['${DEFAULT_COLUMN_PREFIX}2', ['\\u{daaa}']]]`,
  },
  {
    description: 'Empty cell',
    tableData: '1\t2\n3\t',
    expectedEnsoExpression: `Table.input [['${DEFAULT_COLUMN_PREFIX}1', ['1', '3']], ['${DEFAULT_COLUMN_PREFIX}2', ['2', Nothing]]]`,
  },
  {
    description: 'Line feed in cell',
    tableData: '1\t"2\n3"\n4\t5',
    expectedEnsoExpression: `Table.input [['${DEFAULT_COLUMN_PREFIX}1', ['1', '4']], ['${DEFAULT_COLUMN_PREFIX}2', ['2\\n3', '5']]]`,
  },
  {
    description: 'Line feed in quoted cell',
    tableData: '1\t4\n2\t"""5\n6"""',
    expectedEnsoExpression: `Table.input [['${DEFAULT_COLUMN_PREFIX}1', ['1', '2']], ['${DEFAULT_COLUMN_PREFIX}2', ['4', '"5\\n6"']]]`,
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
    expectedEnsoExpression: `Table.input [['${DEFAULT_COLUMN_PREFIX}1', [Nothing, '11', '12', '13', '14', '15', '16', '17', '19', '21', '24', '27', '30']], ['${DEFAULT_COLUMN_PREFIX}2', ['36', Nothing, Nothing, '2.769230769', '2.571428571', '2.4', '2.25', '2.117647059', '1.894736842', '1.714285714', '1.5', '1.333333333', '1.2']], ['${DEFAULT_COLUMN_PREFIX}3', ['52', '4.727272727', '4.333333333', '4', '3.714285714', '3.466666667', '3.25', '3.058823529', '2.736842105', '2.476190476', '2.166666667', '1.925925926', Nothing]]]`,
  },
])('Enso expression from Excel data: $description', ({ tableData, expectedEnsoExpression }) => {
  const rows = parseTsvData(tableData)
  expect(rows).not.toBeNull()
  assert(rows != null)
  expect(tableToEnsoExpression(rows)).toEqual(expectedEnsoExpression)
})
