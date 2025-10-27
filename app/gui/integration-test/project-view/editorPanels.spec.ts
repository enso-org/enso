/** @file Tests for the multiline CodeMirror editor panels: documentation editor and code editor. */
import type EditorPageActions from 'integration-test/actions/EditorPageActions'
import type { Page } from 'integration-test/base'
import { expect, type Locator, test } from 'integration-test/base'
import { mockMethodCallInfo, mockUserDefinedFunctionInfo } from './expressionUpdates'
import { DELETE_KEY } from './keyboard'
import * as locate from './locate'

async function goToGraphAndGetDocs(editorPage: EditorPageActions) {
  let docsContent: Locator
  let docsScroller: Locator
  await editorPage.do(async (page) => {
    docsContent = page.getByTestId('documentation-editor-content')
    docsScroller = page.getByTestId('documentation-editor-scroller')
    await expect(docsContent.locator('.cm-line')).toExist()
  })
  return { docsContent: docsContent!, docsScroller: docsScroller! }
}

test.describe('Main method documentation rendering', () => {
  test('Text', async ({ editorPage }) => {
    const { docsContent } = await goToGraphAndGetDocs(editorPage)
    await expect(docsContent).toContainText('The main method')
  })

  test('Images', async ({ editorPage }) => {
    const { docsContent } = await goToGraphAndGetDocs(editorPage)
    await expect(docsContent.getByAltText('Image')).toHaveCount(3)
    for (const img of await docsContent.getByAltText('Image').all())
      await expect(img).toHaveJSProperty('naturalWidth', 3)
  })

  test('Video', async ({ editorPage }) => {
    const { docsContent } = await goToGraphAndGetDocs(editorPage)
    await expect(docsContent.locator('.DocumentationVideo')).toHaveCount(1)
  })

  test('Lists', async ({ editorPage }) => {
    const { docsContent } = await goToGraphAndGetDocs(editorPage)

    // Nested lists are rendered with hierarchical indentation
    const listItemPos = (text: string) =>
      docsContent
        .locator('span.cm-BulletList-item span')
        .getByText(text, { exact: true })
        .boundingBox()
    const listLevel0 = await listItemPos('Outer list element')
    const listLevel1 = await listItemPos('Nested list element')
    const listLevel2 = await listItemPos('Very nested list element')
    expect(listLevel0).not.toBeNull()
    expect(listLevel1).not.toBeNull()
    expect(listLevel2).not.toBeNull()
    expect(listLevel0!.x).toBeLessThan(listLevel1!.x)
    expect(listLevel1!.x).toBeLessThan(listLevel2!.x)
  })

  test('Link (rendered and interactive)', async ({ editorPage, page, context }) => {
    const { docsContent } = await goToGraphAndGetDocs(editorPage)
    await expect(docsContent.locator('a')).toHaveAccessibleDescription(
      /Click to edit.*Click to open link/,
    )

    await expect(docsContent.locator('a')).toHaveText('https://example.com')
    await docsContent.locator('a').click()
    await expect(page.locator('.LinkEditPopup')).toBeVisible()
    await locate.graphEditor(page).click()
    await expect(page.locator('.LinkEditPopup')).toBeHidden()
    await context.route('https://example.com', (route) =>
      route.fulfill({ status: 200, body: 'YAY' }),
    )
    const newPagePromise = context.waitForEvent('page', { timeout: 10000 })
    await docsContent.locator('a').click({ modifiers: ['ControlOrMeta'] })
    await expect(newPagePromise).resolves.toHaveURL('https://example.com')
  })
})

test.describe('Panels', () => {
  test('Doc panel: Close with hotkey', async ({ editorPage, page }) => {
    const { docsContent } = await goToGraphAndGetDocs(editorPage)
    await page.keyboard.press(`ControlOrMeta+D`)
    await expect(docsContent).toBeHidden()
  })

  test('Doc panel focus (regression #10471)', async ({ editorPage, page }) => {
    const { docsContent } = await goToGraphAndGetDocs(editorPage)

    // Open and focus code editor.
    const { codeEditor, getCodeEditorContent } = await openCodeEditor(page)
    await codeEditor.click()

    await page.evaluate(() => {
      const codeEditorApi = (window as any).__codeEditorApi
      const docStart = codeEditorApi.indexOf('The main method')
      codeEditorApi.placeCursor(docStart + 8)
    })
    await page.keyboard.type(' TEST')

    const content = await getCodeEditorContent()
    expect(content.includes('The main TEST method')).toBe(true)
    await expect(docsContent).toContainText('The main TEST method')
  })

  test('Code editor with wide content does not take space from doc editor (#12476)', async ({
    editorPage,
    page,
  }) => {
    await goToGraphAndGetDocs(editorPage)
    const rightDock = locate.rightDock(page)

    const getDocX = async () => {
      await rightDock.elementHandle().then((el) => el!.waitForElementState('stable'))
      return (await rightDock.boundingBox())!.x
    }
    const docPosWithoutCodeEditor = await getDocX()
    await page.keyboard.press(`ControlOrMeta+\``)
    const codeEditor = page.locator('.CodeEditor')
    await expect(codeEditor).toBeVisible()
    const docPosWithCodeEditor = await getDocX()

    // Note that we compare `x` instead of `width`: This will catch either a change in width, or the
    // viewport becoming larger than the page (causing a change in *apparent* width).
    expect(docPosWithCodeEditor).toBe(docPosWithoutCodeEditor)
  })

  test('Remember scroll position when closed and reopened', async ({ editorPage, page }) => {
    const { docsContent, docsScroller } = await goToGraphAndGetDocs(editorPage)
    const { getCodeEditorContent } = await openCodeEditor(page)

    await setDocumentationText('Some text\n'.repeat(200), {
      docsContent,
      getCodeEditorContent,
      page,
    })
    await docsContent.hover()
    await page.mouse.wheel(0, -2000)
    await docsContent.elementHandle().then((el) => el!.waitForElementState('stable'))
    await page.mouse.wheel(0, 400)
    await docsContent.elementHandle().then((el) => el!.waitForElementState('stable'))
    await expect.poll(() => docsScroller.evaluate((e) => e.scrollTop)).toBe(400)

    await page.keyboard.press(`ControlOrMeta+D`)
    await expect(docsContent).toBeHidden()

    await page.keyboard.press(`ControlOrMeta+D`)
    await expect(docsContent).toBeVisible()
    await expect
      .poll(async () => Math.abs(400 - (await docsScroller.evaluate((e) => e.scrollTop))))
      .toBeLessThan(10)
  })
})

async function setDocumentationText(
  text: string,
  {
    docsContent,
    getCodeEditorContent,
    page,
  }: {
    docsContent: Locator
    getCodeEditorContent: () => Promise<string>
    page: Page
  },
) {
  await docsContent.focus()
  await page.keyboard.press(`ControlOrMeta+A`)
  await page.keyboard.press(DELETE_KEY)
  await docsContent.fill(text)
  const codeAfterSettingNewDocs = await getCodeEditorContent()
  // Wrapping-aware comparison; not correct for all inputs, but it only needs to work for test
  // cases.
  const normalizeWhitespace = (text: string) => text.replaceAll(/\s+/g, ' ')
  expect(normalizeWhitespace(codeAfterSettingNewDocs)).toContain(normalizeWhitespace(text))
}

async function openCodeEditor(page: Page) {
  await page.keyboard.press(`ControlOrMeta+\``)
  const codeEditor = page.locator('.CodeEditor')
  await expect(codeEditor).toBeVisible()
  const getCodeEditorContent = () =>
    page.evaluate(() => (window as any).__codeEditorApi.textContent())
  const codeScroller = codeEditor.locator('.cm-scroller')
  return { codeEditor, getCodeEditorContent, codeScroller }
}

test('Component help', async ({ editorPage, page }) => {
  await editorPage

  await page.getByRole('tab', { name: 'Help' }).click()
  await expect(locate.rightDock(page)).toHaveText(/Select a single component/)

  await locate.graphNodeByBinding(page, 'final').click()
  await expect(locate.rightDock(page)).toHaveText(/No documentation available/)

  await mockMethodCallInfo(page, 'data', {
    methodPointer: {
      module: 'Standard.Base.Data',
      definedOnType: 'Standard.Base.Data',
      name: 'read',
    },
    notAppliedArguments: [0, 1, 2],
  })
  await locate.graphNodeByBinding(page, 'data').click()
  await expect(locate.rightDock(page)).toHaveText(/Reads a file into Enso/)
})

test.describe('User-defined component documentation', () => {
  async function enterUdc(page: Page) {
    await mockUserDefinedFunctionInfo(page, 'final', 'func1')
    await locate.graphNodeByBinding(page, 'final').dblclick()
    await expect(locate.navBreadcrumb(page)).toHaveText(['Mock Project', 'func1'])
  }

  test('Entering function', async ({ editorPage, page }) => {
    const { docsContent } = await goToGraphAndGetDocs(editorPage)
    await enterUdc(page)
    await expect(docsContent).toHaveText('A User Defined Function')
  })

  class FunctionSignatureEditor {
    private constructor(private readonly locator: Locator) {}

    static async new(within: Locator): Promise<FunctionSignatureEditor> {
      const locator = within.locator('.FunctionSignatureEditor')
      await locator.waitFor()
      return new FunctionSignatureEditor(locator)
    }

    async expectArguments(count: number): Promise<FunctionSignatureEditorArgument[]> {
      const argLocators = this.locator.locator('.ArgumentRow')
      await expect(argLocators).toHaveCount(count)
      const args = await argLocators.all()
      expect(args.length).toBe(count)
      return args.map(
        (locator) => new FunctionSignatureEditorArgument(locator, { popoverRoot: this.locator }),
      )
    }
  }

  type MissingBehaviour = 'required' | 'optional' | 'default'

  class FunctionSignatureEditorArgument {
    public readonly defaultValue: Locator
    private readonly popoverRoot: Locator
    private readonly missingBehaviour: Locator
    constructor(
      private readonly locator: Locator,
      { popoverRoot }: { popoverRoot: Locator },
    ) {
      this.defaultValue = this.locator.getByTestId('missing-default-value')
      this.popoverRoot = popoverRoot
      this.missingBehaviour = locator.getByTestId('missing-behaviour')
    }

    /** Check that the current missing-argument behaviour is as specified. */
    expectMissingBehaviour(behaviour: MissingBehaviour): Promise<void> {
      return expect(this.missingBehaviour).toHaveText(behaviour)
    }

    /**
     * Use the dropdown to select a behaviour when the argument is omitted. Verifies that the new
     * behaviour has been set before returning.
     */
    async setMissingBehaviour(behaviour: MissingBehaviour): Promise<void> {
      await this.missingBehaviour.click()
      const dropdown = this.popoverRoot.locator('.DropdownWidget')
      await expect(dropdown).toExist()
      const items = dropdown.locator('.item')
      const item = items.getByText(behaviour)
      await item.click()
      await this.expectMissingBehaviour(behaviour)
      await expect(dropdown).toBeHidden()
      await this.expectMissingBehaviour(behaviour)
    }
  }

  test('Changing argument default', async ({ editorPage, page }) => {
    await goToGraphAndGetDocs(editorPage)
    await enterUdc(page)
    const fse = await FunctionSignatureEditor.new(locate.rightDock(page))
    const [arg] = await fse.expectArguments(1)
    await arg!.expectMissingBehaviour('optional')
    await expect(arg!.defaultValue).toBeHidden()
    await arg!.setMissingBehaviour('required')
    await expect(arg!.defaultValue).toBeHidden()
    await arg!.setMissingBehaviour('optional')
    await expect(arg!.defaultValue).toBeHidden()
    await arg!.setMissingBehaviour('default')
    await expect(arg!.defaultValue).toBeVisible()

    // Regression test for #13627: Ensure missing-behaviour can be changed when the default value
    // widget is focused.
    await arg!.defaultValue.locator('.WidgetEnsoExpression').click()
    await expect(arg!.defaultValue.locator('.WidgetEnsoExpression .cm-content')).toBeFocused()
    await arg!.setMissingBehaviour('optional')
  })
})

test('Insert link button inserts link and focuses editor', async ({ editorPage, page }) => {
  const { docsContent } = await goToGraphAndGetDocs(editorPage)
  const rightDock = locate.rightDock(page)

  // Delete all text and defocus the editor
  await docsContent.locator('.cm-line').first().click()
  await page.keyboard.press(`ControlOrMeta+A`)
  await page.keyboard.press(DELETE_KEY)
  await expect(docsContent.locator('.cm-line')).toBeEmpty()
  await docsContent.blur()

  // Push the button
  await rightDock.getByTestId('action:documentationEditor.link').click()

  // The link exists and is being edited
  await expect(docsContent.locator('a')).toExist()
  await expect(page.locator('.LinkEditPopup')).toExist()
})

test('Documentation editor: Editing with keyboard', async ({ editorPage, page }) => {
  const { docsContent } = await goToGraphAndGetDocs(editorPage)
  const { getCodeEditorContent } = await openCodeEditor(page)

  await docsContent
    .locator('.cm-line')
    .getByText(/The main method/)
    .click()
  await expect(docsContent).toBeFocused()

  await page.keyboard.press(`ControlOrMeta+A`)
  const NEW_DOCS = 'New main method documentation'
  await page.keyboard.type(NEW_DOCS)
  const codeAfterSettingNewDocs = await getCodeEditorContent()
  expect(codeAfterSettingNewDocs).toContain(`## ${NEW_DOCS}`)

  await page.keyboard.press(`ControlOrMeta+Alt+1`)
  const codeAfterHeaderCommand = await getCodeEditorContent()
  expect(codeAfterHeaderCommand).toContain(`## # ${NEW_DOCS}`)

  await page.keyboard.press('Enter')
  await page.keyboard.type('Second line')
  const codeAfterAddingLine = await getCodeEditorContent()
  expect(codeAfterAddingLine).toContain(`## # ${NEW_DOCS}\n   Second line`)
})

function getScrollbarState(locator: Locator) {
  return locator.evaluate((el) => ({
    scrollableWidth: el.scrollWidth > el.clientWidth,
    scrollableHeight: el.scrollHeight > el.clientHeight,
  }))
}

test('Scrollbars in editor panels', async ({ editorPage, page }) => {
  const { docsContent, docsScroller } = await goToGraphAndGetDocs(editorPage)
  await docsContent
    .locator('.cm-line')
    .getByText(/The main method/)
    .click()
  await editorPage.press('Mod+A')
  const NEW_DOCS = ('very long documentation '.repeat(12) + '\n').repeat(35)
  await docsContent.fill(NEW_DOCS)
  await expect(docsContent).toHaveText(NEW_DOCS)
  const docsScrollbars = await getScrollbarState(docsScroller)
  expect(docsScrollbars).toEqual({ scrollableWidth: false, scrollableHeight: true })

  const { codeScroller } = await openCodeEditor(page)
  const codeScrollbars = await getScrollbarState(codeScroller)
  expect(codeScrollbars).toEqual({ scrollableWidth: true, scrollableHeight: true })
})

test.skip('Code editor: Copy and paste (clipboard cannot be used in CI but test can be run locally)', async ({
  editorPage,
  page,
}) => {
  await editorPage
  const { codeEditor, getCodeEditorContent } = await openCodeEditor(page)
  await codeEditor.click()
  await page.evaluate(() => {
    const codeEditor = (window as any).__codeEditorApi
    const PLUS_TEN = ' + ten'
    const plusTen = codeEditor.indexOf(PLUS_TEN)
    codeEditor.select(plusTen, plusTen + PLUS_TEN.length)
  })
  await editorPage.press('Mod+C')
  await editorPage.press('Mod+V')
  await editorPage.press('Mod+V')
  const codeAfterEdit = await getCodeEditorContent()
  expect(codeAfterEdit).toContain(' + ten + ten')
})
