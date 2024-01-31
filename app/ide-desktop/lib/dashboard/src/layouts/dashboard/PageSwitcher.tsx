/** @file Switcher to choose the currently visible full-screen page. */
import * as React from 'react'

import DriveIcon from 'enso-assets/drive.svg'
import HomeIcon from 'enso-assets/home.svg'
import NetworkIcon from 'enso-assets/network.svg'

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
const ERRORS: Record<Page, string | null> = {
  [Page.home]: null,
  [Page.drive]: null,
  [Page.editor]: 'No project is currently open.',
  [Page.settings]: null,
}

/** Data describing how to display a button for a pageg. */
interface PageUIData {
  page: Page
  icon: string
  alt: string
}

const PAGE_DATA: PageUIData[] = [
  { page: Page.home, icon: HomeIcon, alt: 'Go to home page' },
  { page: Page.drive, icon: DriveIcon, alt: 'Go to drive page' },
  { page: Page.editor, icon: NetworkIcon, alt: 'Go to editor page' },
]

/** Props for a {@link PageSwitcher}. */
export interface PageSwitcherProps {
  page: Page
  setPage: (page: Page) => void
  isEditorDisabled: boolean
}

/** Switcher to choose the currently visible full-screen page. */
export default function PageSwitcher(props: PageSwitcherProps) {
  const { page, setPage, isEditorDisabled } = props
  return (
    <div
      className={`cursor-default pointer-events-auto flex items-center rounded-full shrink-0 gap-4 ${
        page === Page.editor ? 'bg-frame backdrop-blur-3xl px-3 -mx-3' : ''
      }`}
    >
      {PAGE_DATA.map(pageData => {
        const isDisabled =
          pageData.page === page || (pageData.page === Page.editor && isEditorDisabled)
        return (
          <Button
            key={pageData.page}
            alt={pageData.alt}
            image={pageData.icon}
            active={page === pageData.page}
            disabled={isDisabled}
            error={ERRORS[pageData.page]}
            onClick={() => {
              setPage(pageData.page)
            }}
          />
        )
      })}
    </div>
  )
}
