import * as iter from 'enso-common/src/utilities/data/iter'
import { describe, expect, test } from 'vitest'
import * as Y from 'yjs'
import { assert, assertDefined } from '../../util/assert'
import { parseModule } from '../parse'
import { MutableBodyBlock, MutableFunctionDef, Statement } from '../tree'

describe('Component documentation (plain text)', () => {
  const plaintextDocumentableStatements = [
    // assignment statement
    'x = 1',
    // expression statement (e.g. output component)
    'x',
  ]
  const textCases = [
    {
      source: '## A component comment',
      text: 'A component comment',
    },
    {
      source: '## A multiline\n       component comment',
      text: 'A multiline\ncomponent comment',
    },
  ]
  const cases = plaintextDocumentableStatements.flatMap((statement) =>
    textCases.map((textCase) => ({ statement, ...textCase })),
  )

  test.each(cases)('Enso source comments to normalized text', ({ statement, source, text }) => {
    const moduleSource = `main =\n    ${source}\n    ${statement}`
    const topLevel = parseModule(moduleSource)
    topLevel.module.setRoot(topLevel)
    const main = iter.first(topLevel.statements())
    assert(main instanceof MutableFunctionDef)
    expect(main.name.code()).toBe('main')
    const body = main.body
    assert(body instanceof MutableBodyBlock)
    const nodeStatement = iter.first(body.statements())
    assertDefined(nodeStatement)
    assert(nodeStatement.isStatement())
    expect(statementDocumentation(nodeStatement).toJSON()).toBe(text)
  })

  test.each(cases)('Text to Enso source', ({ statement, source, text }) => {
    const expectedSource = `main =\n    ${source}\n    ${statement}`
    const initialSource = `main =\n    ${statement}`
    const topLevel = parseModule(initialSource)
    topLevel.module.setRoot(topLevel)
    const main = iter.first(topLevel.statements())
    assert(main instanceof MutableFunctionDef)
    expect(main.name.code()).toBe('main')
    const body = main.body
    assert(body instanceof MutableBodyBlock)
    const nodeStatement = iter.first(body.statements())
    assertDefined(nodeStatement)
    assert(nodeStatement.isStatement())
    const docs = statementDocumentation(nodeStatement)
    docs.insert(0, text)
    expect(topLevel.code()).toBe(expectedSource)
  })

  test.each(cases)(
    'Editing different comments with syncToCode ($statement): $source',
    ({ statement, source }) => {
      const functionCode = (docs: string) => `main =\n    ${docs}\n    ${statement}`
      const moduleOriginalSource = functionCode(source)
      const topLevel = parseModule(moduleOriginalSource)
      topLevel.module.setRoot(topLevel)
      assert(topLevel.code() === moduleOriginalSource)
      const moduleEditedSource = functionCode('## Some new docs')
      topLevel.syncToCode(moduleEditedSource)
      expect(topLevel.module.root()?.code()).toBe(moduleEditedSource)
    },
  )

  test.each(cases)(
    'Setting comments to different content with syncToCode',
    ({ statement, source }) => {
      const functionCode = (docs: string) => `main =\n    ${docs}\n    ${statement}`
      const moduleOriginalSource = functionCode('## Original docs')
      const topLevel = parseModule(moduleOriginalSource)
      const module = topLevel.module
      module.setRoot(topLevel)
      assert(module.root()?.code() === moduleOriginalSource)
      const moduleEditedSource = functionCode(source)
      module.syncToCode(moduleEditedSource)
      expect(module.root()?.code()).toBe(moduleEditedSource)
    },
  )

  test('Setting empty markdown content removes comment', () => {
    const originalSourceWithDocComment = 'main =\n    ## Some docs\n    x = 1'
    const functionCodeWithoutDocs = 'main =\n    x = 1'
    const topLevel = parseModule(originalSourceWithDocComment)
    expect(topLevel.code()).toBe(originalSourceWithDocComment)
    const main = iter.first(topLevel.statements())
    assert(main instanceof MutableFunctionDef)
    expect(main.name.code()).toBe('main')
    const body = main.body
    assert(body instanceof MutableBodyBlock)
    const nodeStatement = iter.first(body.statements())
    assertDefined(nodeStatement)
    assert(nodeStatement.isStatement())
    const docs = statementDocumentation(nodeStatement)
    docs.delete(0, docs.length)
    expect(topLevel.code()).toBe(functionCodeWithoutDocs)
  })
})

describe('Function documentation (Markdown)', () => {
  const cases = [
    {
      source: '## My function',
      markdown: 'My function',
    },
    {
      source: '## My function\n\n   Second paragraph',
      markdown: 'My function\nSecond paragraph',
    },
    {
      source: '## Trailing whitespace \n\n   Second paragraph',
      markdown: 'Trailing whitespace \nSecond paragraph',
    },
    {
      source: '## My function\n\n\n   Second paragraph after extra gap',
      markdown: 'My function\n\nSecond paragraph after extra gap',
    },
    {
      source: '## My function\n   with one hard-wrapped paragraph',
      markdown: 'My function with one hard-wrapped paragraph',
      normalized: '## My function with one hard-wrapped paragraph',
    },
    {
      source: '## ICON group\n   My function with an icon',
      markdown: 'ICON group\nMy function with an icon',
    },
    {
      source: [
        '## This paragraph is hard-wrapped because it its contents are very very very very long,',
        'and such long long lines can be inconvenient to work with in most text editors',
        'because no one likes to scroll horizontally',
        'but if it is edited the reprinted version will be hard-wrapped differently,',
        'because apparently someone has gone and wrapped their source code in a manner',
        'not conforming to the Enso syntax specification',
        'which requires line length not to exceed 100 characters.',
      ].join('\n   '),
      markdown: [
        'This paragraph is hard-wrapped because it its contents are very very very very long,',
        'and such long long lines can be inconvenient to work with in most text editors',
        'because no one likes to scroll horizontally',
        'but if it is edited the reprinted version will be hard-wrapped differently,',
        'because apparently someone has gone and wrapped their source code in a manner',
        'not conforming to the Enso syntax specification',
        'which requires line length not to exceed 100 characters.',
      ].join(' '),
      normalized: [
        '## This paragraph is hard-wrapped because it its contents are very very very very long, and such',
        'long long lines can be inconvenient to work with in most text editors because no one likes to',
        'scroll horizontally but if it is edited the reprinted version will be hard-wrapped differently,',
        'because apparently someone has gone and wrapped their source code in a manner not conforming to',
        'the Enso syntax specification which requires line length not to exceed 100 characters.',
      ].join(' '), // TODO: This should be '\n   ' when hard-wrapping is implemented.
    },
    {
      source: '## Table below:\n   | a | b |\n   |---|---|',
      markdown: 'Table below:\n| a | b |\n|---|---|',
    },
    {
      source: '## Table below:\n\n   | a | b |\n   |---|---|',
      markdown: 'Table below:\n\n| a | b |\n|---|---|',
    },
  ]

  test.each(cases)('Enso source comments to normalized markdown', ({ source, markdown }) => {
    const moduleSource = `${source}\nmain =\n    x = 1`
    const topLevel = parseModule(moduleSource)
    topLevel.module.setRoot(topLevel)
    const main = iter.first(topLevel.statements())
    assert(main instanceof MutableFunctionDef)
    expect(main.name.code()).toBe('main')
    expect(main.mutableDocumentationMarkdown().toJSON()).toBe(markdown)
  })

  test.each(cases)('Markdown to Enso source', ({ source, markdown, normalized }) => {
    const functionCode = 'main =\n    x = 1'
    const topLevel = parseModule(functionCode)
    topLevel.module.setRoot(topLevel)
    const main = iter.first(topLevel.statements())
    assert(main instanceof MutableFunctionDef)
    const markdownYText = main.mutableDocumentationMarkdown()
    expect(markdownYText.toJSON()).toBe('')
    markdownYText.insert(0, markdown)
    expect(topLevel.code()).toBe((normalized ?? source) + '\n' + functionCode)
  })

  test.each(cases)('Unedited comments printed verbatim', ({ source, normalized }) => {
    if (normalized == null) return
    const functionCode = `main =\n    x = 1`
    const moduleSource = source + '\n' + functionCode
    const topLevel = parseModule(moduleSource)
    expect(topLevel.code()).not.toBe(normalized + '\n' + functionCode)
    expect(topLevel.code()).toBe(moduleSource)
  })

  test.each(cases)('Editing different comments with syncToCode', ({ source }) => {
    const functionCode = (docs: string) => `${docs}\nmain =\n    x = 1`
    const moduleOriginalSource = functionCode(source)
    const topLevel = parseModule(moduleOriginalSource)
    topLevel.module.setRoot(topLevel)
    assert(topLevel.code() === moduleOriginalSource)
    const moduleEditedSource = functionCode('Some new docs')
    topLevel.syncToCode(moduleEditedSource)
    expect(topLevel.code()).toBe(moduleEditedSource)
  })

  test.each(cases)('Setting comments to different content with syncToCode', ({ source }) => {
    const functionCode = (docs: string) => `${docs}\nmain =\n    x = 1`
    const moduleOriginalSource = functionCode('## Original docs')
    const topLevel = parseModule(moduleOriginalSource)
    const module = topLevel.module
    module.setRoot(topLevel)
    assert(module.root()?.code() === moduleOriginalSource)
    const moduleEditedSource = functionCode(source)
    module.syncToCode(moduleEditedSource)
    expect(module.root()?.code()).toBe(moduleEditedSource)
  })

  test('Setting empty markdown content removes comment', () => {
    const functionCodeWithoutDocs = `main =\n    x = 1`
    const originalSourceWithDocComment = '## Some docs\n' + functionCodeWithoutDocs
    const topLevel = parseModule(originalSourceWithDocComment)
    expect(topLevel.code()).toBe(originalSourceWithDocComment)

    const main = iter.first(topLevel.statements())
    assert(main instanceof MutableFunctionDef)
    const markdownYText = main.mutableDocumentationMarkdown()
    markdownYText.delete(0, markdownYText.length)
    expect(topLevel.code()).toBe(functionCodeWithoutDocs)
  })
})

function statementDocumentation(statement: Statement): Y.Text {
  assert('mutableDocumentationText' in statement)
  const docs = statement.mutableDocumentationText()
  assertDefined(docs)
  return docs
}
