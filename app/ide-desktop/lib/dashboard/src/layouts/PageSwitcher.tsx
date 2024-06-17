/** @file Switcher to choose the currently visible full-screen page. */
import * as React from 'react'

import DriveIcon from 'enso-assets/drive.svg'
import WorkspaceIcon from 'enso-assets/workspace.svg'

import type * as text from '#/text'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import FocusArea from '#/components/styled/FocusArea'

import * as tailwindMerge from '#/utilities/tailwindMerge'

// ============
// === Page ===
// ============

/** Main content of the screen. Only one should be visible at a time. */
export enum Page {
  drive = 'drive',
  editor = 'editor',
  settings = 'settings',
}

// =================
// === Constants ===
// =================

/** The amount of space that should be left aside for the OS to render the window controls. */
const WINDOW_BUTTONS_WIDTH_LEFT_PX = 0

const PAGE_DATA: PageUIData[] = [
  { page: Page.drive, icon: DriveIcon, nameId: 'drivePageName' },
  { page: Page.editor, icon: WorkspaceIcon, nameId: 'editorPageName' },
]

// ==================
// === PageUIData ===
// ==================

/** Data describing how to display a button for a page. */
interface PageUIData {
  readonly page: Page
  readonly icon: string
  readonly nameId: Extract<text.TextId, `${Page}PageName`>
}

// ====================
// === PageSwitcher ===
// ====================

/** Props for a {@link PageSwitcher}. */
export interface PageSwitcherProps {
  readonly page: Page
  readonly setPage: (page: Page) => void
  readonly isEditorDisabled: boolean
}

/** Switcher to choose the currently visible full-screen page. */
export default function PageSwitcher(props: PageSwitcherProps) {
  const { page, setPage, isEditorDisabled } = props
  const { getText } = textProvider.useText()
  const selectedChildIndexRef = React.useRef(0)
  const lastChildIndexRef = React.useRef(0)
  const visiblePageData = React.useMemo(
    () => PAGE_DATA.filter(pageData => (pageData.page !== Page.editor ? true : !isEditorDisabled)),
    [isEditorDisabled]
  )
  const pageIndexRaw = visiblePageData.findIndex(pageData => page === pageData.page)
  const pageIndex = pageIndexRaw === -1 ? null : pageIndexRaw
  const isLastPageSelected = pageIndexRaw === visiblePageData.length - 1

  React.useEffect(() => {
    selectedChildIndexRef.current = PAGE_DATA.findIndex(data => data.page === page)
  }, [page])

  React.useEffect(() => {
    if (isEditorDisabled) {
      lastChildIndexRef.current = PAGE_DATA.length - 2
    } else {
      lastChildIndexRef.current = PAGE_DATA.length - 1
    }
  }, [isEditorDisabled])

  return (
    <FocusArea direction="horizontal">
      {innerProps => (
        <div
          className="pointer-events-auto flex h-12 shrink-0 grow cursor-default items-center rounded-full"
          {...innerProps}
        >
          <div
            className={tailwindMerge.twMerge(
              'h-full bg-primary/5',
              pageIndex === 0 && 'rounded-br-3xl'
            )}
            style={{ width: WINDOW_BUTTONS_WIDTH_LEFT_PX }}
          />
          {visiblePageData.map((pageData, i) => {
            const active = page === pageData.page
            return (
              <div
                key={pageData.page}
                className={tailwindMerge.twMerge(
                  'h-full transition-[padding-left]',
                  page !== pageData.page && 'bg-primary/5 hover:enabled:bg-primary/[2.5%]',
                  active && 'clip-path-0 rounded-t-3xl outline outline-[1rem] outline-primary/5',
                  pageIndex != null && i === pageIndex + 1 && 'rounded-bl-3xl',
                  pageIndex != null && i === pageIndex - 1 && 'rounded-br-3xl'
                )}
              >
                <ariaComponents.Button
                  size="custom"
                  variant="custom"
                  icon={pageData.icon}
                  className={tailwindMerge.twMerge(
                    'flex h-full items-center gap-3 pr-4 selectable',
                    active && 'disabled active',
                    page === pageData.page ? 'pl-[19px]' : 'pl-4'
                  )}
                  onPress={() => {
                    setPage(pageData.page)
                  }}
                >
                  {getText(pageData.nameId)}
                </ariaComponents.Button>
              </div>
            )
          })}
          <div
            className={tailwindMerge.twMerge(
              'h-full grow bg-primary/5',
              isLastPageSelected && 'rounded-bl-3xl'
            )}
          />
        </div>
      )}
    </FocusArea>
  )
}
