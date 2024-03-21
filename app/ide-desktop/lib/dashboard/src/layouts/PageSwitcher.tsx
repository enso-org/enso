/** @file Switcher to choose the currently visible full-screen page. */
import * as React from 'react'

import DriveIcon from 'enso-assets/drive.svg'
import HomeIcon from 'enso-assets/home.svg'
import NetworkIcon from 'enso-assets/network.svg'

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
const ERRORS: Readonly<Record<Page, string | null>> = {
  [Page.home]: null,
  [Page.drive]: null,
  [Page.editor]: 'No project is currently open.',
  [Page.settings]: null,
}

/** Data describing how to display a button for a pageg. */
interface PageUIData {
  readonly page: Page
  readonly icon: string
  readonly alt: string
}

const PAGE_DATA: PageUIData[] = [
  { page: Page.home, icon: HomeIcon, alt: 'Go to home page' },
  { page: Page.drive, icon: DriveIcon, alt: 'Go to drive page' },
  { page: Page.editor, icon: NetworkIcon, alt: 'Go to editor page' },
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
      {(ref, innerProps) => (
        <div
          ref={ref}
          className={`pointer-events-auto flex shrink-0 cursor-default items-center gap-pages rounded-full px-page-switcher-x ${
            page === Page.editor ? 'bg-frame backdrop-blur-default' : ''
          }`}
          {...innerProps}
        >
          {PAGE_DATA.map(pageData => {
            return (
              <Button
                key={pageData.page}
                alt={pageData.alt}
                image={pageData.icon}
                active={page === pageData.page}
                softDisabled={page === pageData.page}
                isDisabled={pageData.page === Page.editor && isEditorDisabled}
                error={ERRORS[pageData.page]}
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
