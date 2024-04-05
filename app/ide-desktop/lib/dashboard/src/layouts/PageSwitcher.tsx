/** @file Switcher to choose the currently visible full-screen page. */
import * as React from 'react'

import DriveIcon from 'enso-assets/drive.svg'
import NetworkIcon from 'enso-assets/network.svg'

import type * as text from '#/text'

import * as textProvider from '#/providers/TextProvider'

import Button from '#/components/Button'

// ====================
// === PageSwitcher ===
// ====================

/** Main content of the screen. Only one should be visible at a time. */
export enum Page {
  home = 'home',
  drive = 'drive',
  editor = 'editor',
  settings = 'settings',
}

/** Error text for each page. */
const ERRORS = {
  [Page.home]: null,
  [Page.drive]: null,
  [Page.editor]: 'noProjectIsCurrentlyOpen',
  [Page.settings]: null,
} as const satisfies Record<Page, text.TextId | null>

const PAGE_TO_ALT_TEXT_ID: Readonly<Record<Page, text.TextId>> = {
  home: 'homePageAltText',
  drive: 'drivePageAltText',
  editor: 'editorPageAltText',
  settings: 'settingsPageAltText',
} satisfies { [P in Page]: `${P}PageAltText` }

const PAGE_TO_TOOLTIP_ID: Readonly<Record<Page, text.TextId>> = {
  home: 'homePageTooltip',
  drive: 'drivePageTooltip',
  editor: 'editorPageTooltip',
  settings: 'settingsPageTooltip',
} satisfies { [P in Page]: `${P}PageTooltip` }

/** Data describing how to display a button for a page. */
interface PageUIData {
  readonly page: Page
  readonly icon: string
}

const PAGE_DATA: PageUIData[] = [
  { page: Page.drive, icon: DriveIcon },
  { page: Page.editor, icon: NetworkIcon },
]

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

  return (
    <div
      className={`pointer-events-auto flex shrink-0 cursor-default items-center gap-pages rounded-full px-page-switcher-x ${
        page === Page.editor ? 'bg-frame backdrop-blur-default' : ''
      }`}
    >
      {PAGE_DATA.map(pageData => {
        const isDisabled = pageData.page === Page.editor && isEditorDisabled
        const errorId = ERRORS[pageData.page]
        return (
          <Button
            key={pageData.page}
            image={pageData.icon}
            active={page === pageData.page}
            alt={getText(PAGE_TO_ALT_TEXT_ID[pageData.page])}
            title={getText(PAGE_TO_TOOLTIP_ID[pageData.page])}
            disabled={isDisabled}
            error={errorId == null ? null : getText(errorId)}
            onClick={() => {
              setPage(page === pageData.page ? Page.home : pageData.page)
            }}
          />
        )
      })}
    </div>
  )
}
