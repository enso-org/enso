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
  drive = 'drive',
  editor = 'editor',
  settings = 'settings',
}

/** Error text for each page. */
const ERRORS = {
  [Page.drive]: null,
  [Page.editor]: 'noProjectIsCurrentlyOpen',
  [Page.settings]: null,
} as const satisfies Record<Page, text.TextId | null>

/** Data describing how to display a button for a page. */
interface PageUIData {
  readonly page: Page
  readonly icon: string
  readonly altId: Extract<text.TextId, `${Page}PageAltText`>
}

const PAGE_DATA: PageUIData[] = [
  { page: Page.drive, icon: DriveIcon, altId: 'drivePageAltText' },
  {
    page: Page.editor,
    icon: NetworkIcon,
    altId: 'editorPageAltText',
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
  const selectedChildIndexRef = React.useRef(0)
  const lastChildIndexRef = React.useRef(0)

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
          className={`pointer-events-auto flex shrink-0 cursor-default items-center gap-pages rounded-full px-page-switcher-x ${
            page === Page.editor ? 'bg-frame backdrop-blur-default' : ''
          }`}
          {...innerProps}
        >
          {PAGE_DATA.map(pageData => {
            const error = ERRORS[pageData.page]
            return (
              <Button
                key={pageData.page}
                alt={getText(pageData.altId)}
                image={pageData.icon}
                active={page === pageData.page}
                softDisabled={page === pageData.page}
                isDisabled={pageData.page === Page.editor && isEditorDisabled}
                error={error == null ? null : getText(error)}
                onPress={() => {
                  setPage(pageData.page)
                }}
              />
            )
          })}
        </div>
      )}
    </FocusArea>
  )
}
