import { useComponentBrowserInput } from '@/components/ComponentBrowser/input'
import { GraphDb, NodeId } from '@/stores/graph/graphDatabase'
import { ComputedValueRegistry } from '@/stores/project/computedValueRegistry'
import { SuggestionDb } from '@/stores/suggestionDatabase'
import { unwrap } from '@/util/data/result'
import { parseAbsoluteProjectPathRaw } from '@/util/projectPath'
import { expect, test } from 'vitest'
import { assert, assertUnreachable } from 'ydoc-shared/util/assert'
import { Range } from 'ydoc-shared/util/data/range'

const aiMock = { query: assertUnreachable }
const operator1Id = '3d0e9b96-3ca0-4c35-a820-7d3a1649de55' as NodeId
const operator2Id = '5eb16101-dd2b-4034-a6e2-476e8bfa1f2b' as NodeId

function mockGraphDb() {
  const computedValueRegistryMock = ComputedValueRegistry.Mock()
  computedValueRegistryMock.db.set(operator1Id, {
    typename: unwrap(parseAbsoluteProjectPathRaw('Standard.Base.Number')),
    rawTypename: 'Standard.Base.Number',
    methodCall: undefined,
    payload: { type: 'Value' },
    profilingInfo: [],
  })
  const db = GraphDb.Mock(computedValueRegistryMock)
  db.mockNode('operator1', operator1Id, 'Data.read')
  db.mockNode('operator2', operator2Id)
  return db
}

test.each`
  inputContent   | expectedLiteral
  ${'read'}      | ${undefined}
  ${'operator1'} | ${undefined}
  ${'12 + 14'}   | ${undefined}
  ${'12'}        | ${'12'}
  ${'12.6'}      | ${'12.6'}
  ${'"text"'}    | ${'"text"'}
  ${"'text'"}    | ${"'text'"}
  ${"'text"}     | ${"'text'"}
  ${"'''text"}   | ${"'''text"}
`('Reading literal from $inputContent', ({ inputContent, expectedLiteral }) => {
  const input = useComponentBrowserInput(mockGraphDb(), new SuggestionDb(), aiMock)
  input.reset({ type: 'newNode' })
  input.content = { text: inputContent, selection: Range.empty }
  assert(input.mode.mode === 'componentBrowsing')
  expect(input.mode.literal?.code()).toBe(expectedLiteral)
})
