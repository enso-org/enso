/** @file Switcher to choose the currently visible full-screen page. */
import * as React from 'react'

import DriveIcon from 'enso-assets/drive.svg'
import NetworkIcon from 'enso-assets/network.svg'

import type * as text from '#/text'

import * as textProvider from '#/providers/TextProvider'

import Button from '#/components/styled/Button'
import FocusArea from '#/components/styled/FocusArea'

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

/** Data describing how to display a button for a page. */
interface PageUIData {
  readonly page: Page
  readonly icon: string
  readonly altId: Extract<text.TextId, `${Page}PageAltText`>
  readonly tooltipId: Extract<text.TextId, `${Page}PageTooltip`>
}

const PAGE_DATA: PageUIData[] = [
  { page: Page.drive, icon: DriveIcon, altId: 'drivePageAltText', tooltipId: 'drivePageTooltip' },
  {
    page: Page.editor,
    icon: NetworkIcon,
    altId: 'editorPageAltText',
    tooltipId: 'editorPageTooltip',
  },
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
    <FocusArea direction="horizontal">
      {innerProps => (
        <div
          className={`pointer-events-auto flex shrink-0 cursor-default items-center gap-pages rounded-full px-page-switcher-x ${
            page === Page.editor ? 'bg-frame backdrop-blur-default' : ''
          }`}
          {...innerProps}
        >
          {PAGE_DATA.map(pageData => {
            return (
              <Button
                key={pageData.page}
                aria-label={getText(pageData.tooltipId)}
                alt={getText(pageData.altId)}
                image={pageData.icon}
                active={page === pageData.page}
                isDisabled={pageData.page === Page.editor && isEditorDisabled}
                error={ERRORS[pageData.page]}
                onPress={() => {
                  setPage(page === pageData.page ? Page.home : pageData.page)
                }}
              />
            )
          })}
        </div>
      )}
    </FocusArea>
  )
}
