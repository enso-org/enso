/** @file Switcher to choose the currently visible full-screen page. */
import * as React from 'react'

import clsx from 'clsx'

import DriveIcon from 'enso-assets/drive.svg'
import NetworkIcon from 'enso-assets/network.svg'

import type * as text from '#/text'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import FocusArea from '#/components/styled/FocusArea'
import SvgMask from '#/components/SvgMask'
import UnstyledButton from '#/components/UnstyledButton'

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

const PAGE_DATA: PageUIData[] = [
  { page: Page.drive, icon: DriveIcon, nameId: 'drivePageName' },
  {
    page: Page.editor,
    icon: NetworkIcon,
    nameId: 'editorPageName',
  },
]

//===================
// === PageUIData ===
//===================

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
  const pageIndexRaw = PAGE_DATA.findIndex(pageData => page === pageData.page)
  const pageIndex = pageIndexRaw === -1 ? null : pageIndexRaw
  const isLastPageSelected = pageIndexRaw === PAGE_DATA.length - 1

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
          {PAGE_DATA.map((pageData, i) => {
            const active = page === pageData.page
            return (
              <div
                key={pageData.page}
                className={clsx(
                  'h-full px-4',
                  page !== pageData.page && 'bg-primary/5 hover:enabled:bg-primary/[2.5%]',
                  active && 'clip-path-0 rounded-t-3xl outline outline-[1rem] outline-primary/5',
                  pageIndex != null && i === pageIndex + 1 && 'rounded-bl-3xl',
                  pageIndex != null && i === pageIndex - 1 && 'rounded-br-3xl'
                )}
              >
                <UnstyledButton
                  className={`flex h-full items-center gap-icon-with-text px-4 selectable ${active ? 'disabled active' : ''}`}
                  isDisabled={pageData.page === Page.editor && isEditorDisabled}
                  onPress={() => {
                    setPage(pageData.page)
                  }}
                >
                  <SvgMask src={pageData.icon} />
                  <aria.Text className="text">{getText(pageData.nameId)}</aria.Text>
                </UnstyledButton>
              </div>
            )
          })}
          <div
            className={`h-full grow bg-primary/5 ${isLastPageSelected ? 'rounded-bl-3xl' : ''}`}
          />
        </div>
      )}
    </FocusArea>
  )
}
