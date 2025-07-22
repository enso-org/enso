import {
  parseTestInput,
  printTestInput,
  setupEditor,
} from '@/components/MarkdownEditor/__tests__/testInput'
import {
  canInsertLink,
  getInlineFormatting,
  type InlineFormattingNode,
  insertLink,
  setInlineFormatting,
} from '@/components/MarkdownEditor/codemirror/formatting/inline'
import { expect, test } from 'vitest'

interface BaseTestCase {
  source: string
}
interface FormattableCase extends BaseTestCase {
  unformattable?: never
  italic?: boolean
  bold?: boolean
  strikethrough?: boolean
  italicToggled?: string
  boldToggled?: string
  strikethroughToggled?: string
}
interface UnformattableCase extends BaseTestCase {
  unformattable: true
}

type TestCase = FormattableCase | UnformattableCase

/**
 * The cursor is inside a word. This is a hybrid case: The "current" formatting is determined by the format tree
 * containing the cursor (i.e. the state is italic if and only if the cursor is within an italic node), but edits are
 * applied to the word containing the cursor.
 */
const flankedCursorCases: FormattableCase[] = [
  {
    source: 'Some| text',
    italicToggled: '*Some|* text',
  },
  {
    source: '## |Some text',
    italicToggled: '## |*Some* text',
  },
  {
    source: 'Some te|xt',
    italicToggled: 'Some *te|xt*',
  },
  {
    source: 'Node removal, cursor inside *delimiters|*',
    italic: true,
    italicToggled: 'Node removal, cursor inside delimiters|',
  },
  {
    source: 'Node removal, cursor outside *delimiters*|',
    // FIXME
    //italic: true,
    //italicToggled: 'Node removal, cursor outside delimiters|',
    italicToggled: 'Node removal, cursor outside *delimiters|*',
  },
  {
    source: 'Some *|*text**',
    bold: true,
    italicToggled: 'Some **|*text***',
    boldToggled: 'Some |text',
  },
  {
    source: '|No*de expansion*',
    italicToggled: '|*Node expansion*',
  },
  {
    source: '*Node expan*sion|',
    italicToggled: '*Node expansion|*',
  },
  {
    source: 'No*de| contraction*',
    italic: true,
    italicToggled: 'Node| *contraction*',
  },
  {
    source: '*Node |con*traction',
    italic: true,
    italicToggled: '*Node* |contraction',
  },
  {
    source: '[Link text](https://example|.com)',
    italicToggled: '*[Link text](https://example|.com)*',
  },
  {
    source: '[Link text](<https://example.com/Url | containing spaces>)',
    italicToggled: '*[Link text](<https://example.com/Url | containing spaces>)*',
  },
  {
    source: '[Link text](<https://example.com/Url with | spaces>)',
    italicToggled: '*[Link text](<https://example.com/Url with | spaces>)*',
  },
  {
    source: '[Link text](<https://example.com/Url with *|* spaces and asterisks>)',
    italicToggled: '*[Link text](<https://example.com/Url with *|* spaces and asterisks>)*',
  },
  {
    source: '*[Link text](<https://example.com/Url with | spaces>)*',
    italic: true,
    italicToggled: '[Link text](<https://example.com/Url with | spaces>)',
  },
  {
    source: '*[Link text](<https://example.com/Url with *|* spaces and asterisks>)*',
    italic: true,
    italicToggled: '[Link text](<https://example.com/Url with *|* spaces and asterisks>)',
  },
  {
    source: '`Inline | code`',
    italicToggled: '*`Inline | code`*',
  },
  {
    source: '`Inline *|* code`',
    italicToggled: '*`Inline *|* code`*',
  },
  {
    source: '`Inline | code` in paragraph',
    italicToggled: '*`Inline | code`* in paragraph',
  },
  {
    source: 'Paragraph contains `inline | code`',
    italicToggled: 'Paragraph contains *`inline | code`*',
  },
]

/**
 * Selections. We don't need to cover range normalization here, as that's tested independently; however range trimming
 * and seminormalization (splitting) are not currently independently tested.
 */
const selectionCases: TestCase[] = [
  {
    source: '|Node| insertion',
    italicToggled: '|*Node*| insertion',
  },
  {
    source: '|Node **insertion**|',
    italicToggled: '|*Node **insertion***|',
  },
  {
    source: 'Node |**insertion**|',
    bold: true,
    italicToggled: 'Node |***insertion***|',
  },
  {
    source: 'Node| **insertion**|',
    bold: true,
    italicToggled: 'Node| ***insertion***|',
  },
  {
    source: 'Node ***|removal|***',
    italic: true,
    bold: true,
    italicToggled: 'Node **|removal|**',
    boldToggled: 'Node *|removal|*',
  },
  {
    source: '|Node insertion *removing inner*|',
    italicToggled: '|*Node insertion removing inner*|',
  },
  {
    source: 'Insert|ion within wor|ds',
    italicToggled: 'Insert|*ion within wor*|ds',
  },
  {
    source: 'Multiline |multiple\nnode\ninsertion|',
    italicToggled: 'Multiline |*multiple*\n*node*\n*insertion*|',
  },
  {
    source: '|Node *expansion| test*',
    italicToggled: '|*Node expansion| test*',
  },
  {
    source: '|Node _expansion| test_, alternate delimiter',
    italicToggled: '|*Node expansion*| _test_, alternate delimiter',
  },
  {
    source: '|Complex *~~node| expansion~~*',
    italicToggled: '|*Complex ~~node| expansion~~*',
  },
  {
    source: '*Double |ended* node *expansion| case*',
    italicToggled: '*Double |ended node expansion| case*',
  },
  {
    source: '*Double |ended* node _expansion| case with incompatible delimiters_',
    italicToggled: '*Double |ended node expansion|* _case with incompatible delimiters_',
  },
  {
    source: '_Double |ended_ node *expansion| case with incompatible delimiters*',
    italicToggled: '_Double_ *|ended node expansion| case with incompatible delimiters*',
  },
  {
    source: 'Node |*removal*|',
    italic: true,
    italicToggled: 'Node |removal|',
  },
  {
    source: '|*Multiple* *node* *removal*|',
    italic: true,
    italicToggled: '|Multiple node removal|',
  },
  {
    source: 'Multiline |*multiple*\n*node*\n*removal*|',
    italic: true,
    italicToggled: 'Multiline |multiple\nnode\nremoval|',
  },
  {
    source: '*Node |contraction|*',
    italic: true,
    italicToggled: '*Node* |contraction|',
  },
  {
    source: '*|Node| contraction*',
    italic: true,
    italicToggled: '|Node| *contraction*',
  },
  {
    source: '*~~Complex node |contraction|~~*',
    italic: true,
    strikethrough: true,
    italicToggled: '*~~Complex node~~* |~~contraction~~|',
  },
  {
    source: '*Outer |node| closing*',
    italic: true,
    italicToggled: '*Outer* |node| *closing*',
  },
  {
    source: '*~~Complex |outer node| closing~~*',
    italic: true,
    strikethrough: true,
    italicToggled: '*~~Complex~~* |~~outer node~~| *~~closing~~*',
    // TODO: Tree-inversion simplifications
    // italicToggled: '~~*Complex* |outer node| *closing*~~',
  },
  {
    source: '*Outer node *closing and |nested* node| contraction*',
    italic: true,
    italicToggled: '*Outer node *closing and** |nested node| *contraction*',
  },
  {
    source: '*No|de closing, cursor inside wor|ds*',
    italic: true,
    italicToggled: '*No*|de closing, cursor inside wor|*ds*',
  },
  {
    source: '*Multiple no|de closing, cursor* *inside wor|ds*',
    italic: true,
    italicToggled: '*Multiple no*|de closing, cursor inside wor|*ds*',
  },
  {
    source: '~~Outer node |splitting~~ case|',
    italicToggled: '~~Outer node~~ |*~~splitting~~ case*|',
  },
  {
    source: '|Outer node ~~splitting| case~~',
    italicToggled: '|*Outer node ~~splitting~~*| ~~case~~',
  },
  {
    source: '~~**Nested outer |node**~~ splitting|',
    italicToggled: '~~**Nested outer**~~ |*~~**node**~~ splitting*|',
  },
  {
    source: '~~Outer node |splitting~~ and *node| expansion*',
    italicToggled: '~~Outer node~~ |*~~splitting~~ and node| expansion*',
  },
  {
    source: '~~*Outer |node*~~ splitting| with inner pseudo- node expansion',
    italicToggled: '~~*Outer*~~ |*~~node~~ splitting*| with inner pseudo- node expansion',
  },
  {
    source: '~~*Some |partly*~~ italic|-and-strikethrough text',
    italicToggled: '~~*Some*~~ |*~~partly~~ italic*|-and-strikethrough text',
  },
  {
    source: '|Partly [link| text](https://example.com)',
    // TODO (AllowFormattingLinkText)
    //italicToggled: '|*Partly* [*link*| text](https://example.com)',
    italicToggled: '|*Partly* [link| text](https://example.com)',
  },
  {
    source: '|*Partly* [*link| text*](https://example.com)',
    italic: true,
    // TODO (AllowFormattingLinkText)
    // italicToggled: '|Partly [link| *text*](https://example.com)',
    italicToggled: '|Partly [*link| text*](https://example.com)',
  },
  {
    source: '[Link text](https://|example|.com)',
    unformattable: true,
  },
  {
    source: '|Partly `unformattable| text`',
    italicToggled: '|*Partly* `unformattable| text`',
  },
  {
    source: '|Partly `unformattable text|`',
    italicToggled: '|*Partly `unformattable text`*|',
  },
  {
    source: '|Partly `unformattable text`|',
    italicToggled: '|*Partly `unformattable text`*|',
  },
  {
    source: '|*Partly `unformattable| text`*',
    italic: true,
    italicToggled: '|Partly *`unformattable| text`*',
  },
  {
    source: '|Partly `unformattable` text|',
    italicToggled: '|*Partly `unformattable` text*|',
  },
  {
    source: '|*Partly `unformattable` text*|',
    italic: true,
    italicToggled: '|Partly `unformattable` text|',
  },
  {
    source: '`Selection |has` two `unformattable| parts`',
    italicToggled: '`Selection |has` *two* `unformattable| parts`',
  },
  {
    source: '`Selection |has` *two* `unformattable| parts`',
    italic: true,
    italicToggled: '`Selection |has` two `unformattable| parts`',
  },
  {
    source: '*`Selection |has` two `unformattable| parts`*',
    italic: true,
    italicToggled: '*`Selection |has`* two *`unformattable| parts`*',
  },
  {
    source: '*Delimiters|*| *only*',
    unformattable: true,
  },
  {
    source: '*Delimiters|* *|only*',
    unformattable: true,
  },
  {
    source: '*Whitespace| |only*',
    unformattable: true,
  },
  {
    source: '|Node| *extension*',
    italicToggled: '*|Node| extension*',
  },
  {
    source: 'Not |node| ~~*extension*~~',
    italicToggled: 'Not |*node*| ~~*extension*~~',
  },
  {
    source: '*Double-ended* |node| *extension*',
    italicToggled: '*Double-ended |node| extension*',
  },
  {
    source: '_Double-ended_ |node| _extension_, alternate delimiter',
    italicToggled: '_Double-ended_ |*node*| _extension_, alternate delimiter',
  },
  {
    source: '*Double-ended* |node| _extension_, incompatible delimiters',
    italicToggled: '*Double-ended |node|* _extension_, incompatible delimiters',
  },
  {
    source: '*Node extension i*|nsid|*e word*',
    italicToggled: '*Node extension i|nsid|e word*',
  },
  {
    source: '*Node extension* |in|to word',
    italicToggled: '*Node extension |in*|to word',
  },
  {
    source: '_Node extension_ |do|es not try to use alternate delimiter inside word',
    italicToggled: '_Node extension_ |*do*|es not try to use alternate delimiter inside word',
  },
  {
    source: '*Node extension i*|~~nsid~~|*e word*',
    strikethrough: true,
    italicToggled: '*Node extension i|~~nsid~~|e word*',
  },
  /*
  { // FIXME: Why does this parse as `italic: true`??
    source: '*~~Node extension i~~*|nsid|*~~e word~~*',
    italicToggled: '*~~Node extension i~~|nsid|~~e word~~*',
  },
   */
  {
    source: [
      'Block |types',
      '# Block types',
      '## Block types',
      '### Block types',
      '> Block types',
      '1. Block types',
      '- Block| types',
    ].join('\n'),
    italicToggled: [
      'Block |*types*',
      '# *Block types*',
      '## *Block types*',
      '### *Block types*',
      '> *Block types*',
      '1. *Block types*',
      '- *Block*| types',
    ].join('\n'),
  },
]

function checkGetFormat(input: TestCase) {
  const { source } = input
  const state = setupEditor(source).state
  const formatting = getInlineFormatting(state)
  expect(
    formatting && {
      italic: formatting.Emphasis,
      bold: formatting.StrongEmphasis,
      strikethrough: formatting.Strikethrough,
    },
  ).toStrictEqual(
    'unformattable' in input ? undefined : (
      {
        italic: !!input.italic,
        bold: !!input.bold,
        strikethrough: !!input.strikethrough,
      }
    ),
  )
}
test.each(flankedCursorCases)('Get format state at cursor: $source', checkGetFormat)
test.each(selectionCases)('Get format state of selection: $source', checkGetFormat)

function checkSetFormat(input: FormattableCase) {
  const { source, italic, bold, strikethrough } = input
  const testSet = (nodeType: InlineFormattingNode, value: boolean, expectedInput: string) => {
    const view = setupEditor(source)
    view.dispatch(setInlineFormatting(view.state, nodeType, value))
    const text = view.state.doc.toString()
    const expected = parseTestInput(expectedInput)
    expect(text).toEqual(expected.doc)
    // TODO (SelectionMapping): Test this unconditionally.
    if (view.state.selection.main.empty) {
      expect(printTestInput(text, view.state.selection.main)).toEqual(expectedInput)
    }
  }
  if (input.italicToggled != null) testSet('Emphasis', !italic, input.italicToggled)
  if (input.boldToggled != null) testSet('StrongEmphasis', !bold, input.boldToggled)
  if (input.strikethroughToggled != null)
    testSet('Strikethrough', !strikethrough, input.strikethroughToggled)
}
function isToggleCase(input: TestCase): input is FormattableCase {
  return (
    !('unformattable' in input) &&
    (input.italicToggled != null || input.boldToggled != null || input.strikethroughToggled != null)
  )
}
test.each(flankedCursorCases.filter(isToggleCase))(
  'Set format at flanked cursor: $source',
  checkSetFormat,
)
test.each(selectionCases.filter(isToggleCase))('Set format of selection: $source', checkSetFormat)

const url = '(|https://|)'
test.each([
  {
    source: 'Insert link at cursor: |',
    expected: `Insert link at cursor: [Link]${url}`,
  },
  {
    source: 'Linkify |selected text|',
    expected: `Linkify [selected text]${url}`,
  },
  {
    source: 'Linkify word| at cursor',
    expected: `Linkify [word]${url} at cursor`,
  },
  {
    source: 'Linkify **word|** at cursor',
    expected: `Linkify [**word**]${url} at cursor`,
  },
  {
    source: 'Linkify |selected *partly-emphasized| text*',
    expected: `Linkify [selected *partly-emphasized*]${url} *text*`,
  },
  {
    source: 'Linkify `inline-unformattable|` text',
    expected: `Linkify [\`inline-unformattable\`]${url} text`,
  },
  {
    source: 'Linkify `partly |unformattable` text|',
    expected: `Linkify \`partly unformattable\` [text]${url}`,
  },
  {
    source: 'Linkify `inside |unformattable| text`',
    expected: undefined,
  },
])('Insert link: $source', ({ source, expected }) => {
  const view = setupEditor(source)
  if (expected === undefined) {
    expect(canInsertLink(view.state)).toBe(false)
  } else {
    expect(canInsertLink(view.state)).toBe(true)
    view.dispatch(insertLink(view.state))
    expect(printTestInput(view.state.doc.toString(), view.state.selection.main)).toEqual(expected)
  }
})
