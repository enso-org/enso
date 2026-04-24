import { expect, test } from 'vitest'
import {
  type Choice,
  DisplayMode,
  multipleChoiceConfiguration,
  singleChoiceConfiguration,
} from '../configuration'

const dynamicConfig = (): { label: string; values: Choice[] } => ({
  label: 'split_at',
  values: [
    {
      label: 'whitespace',
      value: [
        {
          value: 'space',
          label: null,
          parameters: [
            [
              'starting_from',
              { kind: 'Numeric_Input', minimum: 0, maximum: 100, display: DisplayMode.Always },
            ],
          ],
        },
        {
          value: 'newline',
          label: null,
          parameters: [],
        },
      ],
      parameters: [],
    },
    {
      label: 'character',
      value: 'character',
      parameters: [['character', { kind: 'Text_Input', display: DisplayMode.Always }]],
    },
    {
      label: 'regex',
      value: 'regex',
      parameters: [['regex', { kind: 'Text_Input', display: DisplayMode.Always }]],
    },
  ],
})

const expectedPossibleFunctions = new Map([
  [
    'space',
    {
      kind: 'FunctionCall',
      parameters: new Map([
        [
          'starting_from',
          { kind: 'Numeric_Input', minimum: 0, maximum: 100, display: DisplayMode.Always },
        ],
      ]),
    },
  ],
  ['newline', { kind: 'FunctionCall', parameters: new Map() }],
  [
    'character',
    {
      kind: 'FunctionCall',
      parameters: new Map([['character', { kind: 'Text_Input', display: DisplayMode.Always }]]),
    },
  ],
  [
    'regex',
    {
      kind: 'FunctionCall',
      parameters: new Map([['regex', { kind: 'Text_Input', display: DisplayMode.Always }]]),
    },
  ],
])

test('Configuration of the inner widget', () => {
  const singleChoiceInnerConfig = singleChoiceConfiguration({
    kind: 'Single_Choice',
    ...dynamicConfig(),
  })
  expect(singleChoiceInnerConfig).toEqual({
    kind: 'OneOfFunctionCalls',
    possibleFunctions: expectedPossibleFunctions,
  })

  const multipleChoiceInnerConfig = multipleChoiceConfiguration({
    kind: 'Multiple_Choice',
    ...dynamicConfig(),
  })
  expect(multipleChoiceInnerConfig).toEqual({
    kind: 'SomeOfFunctionCalls',
    possibleFunctions: expectedPossibleFunctions,
  })
})
