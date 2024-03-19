/** @file Switcher to choose the currently visible full-screen page. */
import * as React from 'react'

import DriveIcon from 'enso-assets/drive.svg'
import HomeIcon from 'enso-assets/home.svg'
import NetworkIcon from 'enso-assets/network.svg'

import * as keyboardNavigationHooks from '#/hooks/keyboardNavigationHooks'

import * as navigator2DProvider from '#/providers/Navigator2DProvider'

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
const PAGE_DATA_WITHOUT_EDITOR = PAGE_DATA.filter(page => page.page !== Page.editor)

/** Props for a {@link PageSwitcher}. */
export interface PageSwitcherProps {
  readonly page: Page
  readonly setPage: (page: Page) => void
  readonly isEditorDisabled: boolean
}

/** Switcher to choose the currently visible full-screen page. */
export default function PageSwitcher(props: PageSwitcherProps) {
  const { page, setPage, isEditorDisabled } = props
  const rootRef = React.useRef<HTMLDivElement>(null)
  const selectedChildIndexRef = React.useRef(0)
  const navigator2D = navigator2DProvider.useNavigator2D()
  const lastChildIndexRef = React.useRef(0)
  const selectablePageData = isEditorDisabled ? PAGE_DATA_WITHOUT_EDITOR : PAGE_DATA

  const [keyboardSelectedIndex, setKeyboardSelectedIndex] =
    keyboardNavigationHooks.useKeyboardChildNavigation(rootRef, {
      axis: keyboardNavigationHooks.Axis.horizontal,
      length: selectablePageData.length,
      defaultIndex: selectedChildIndexRef.current,
    })

  const keyboardSelectedPage =
    keyboardSelectedIndex == null ? null : selectablePageData[keyboardSelectedIndex]?.page ?? null

  React.useEffect(() => {
    const root = rootRef.current
    if (root == null) {
      return
    } else {
      return navigator2D.register(root, {
        focusPrimaryChild: () => {
          setKeyboardSelectedIndex(selectedChildIndexRef.current)
        },
        focusWhenPressed: {
          right: setKeyboardSelectedIndex.bind(null, 0),
          left: () => {
            setKeyboardSelectedIndex(lastChildIndexRef.current)
          },
        },
      })
    }
  }, [navigator2D, setKeyboardSelectedIndex])

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
    <div
      ref={rootRef}
      className={`pointer-events-auto flex shrink-0 cursor-default items-center gap-pages rounded-full px-page-switcher-x ${
        page === Page.editor ? 'bg-frame backdrop-blur-default' : ''
      }`}
    >
      {PAGE_DATA.map(pageData => {
        return (
          <Button
            key={pageData.page}
            ref={element => {
              if (pageData.page === keyboardSelectedPage) {
                element?.focus()
              }
            }}
            focusRing={pageData.page === keyboardSelectedPage}
            alt={pageData.alt}
            image={pageData.icon}
            active={page === pageData.page}
            softDisabled={page === pageData.page}
            disabled={pageData.page === Page.editor && isEditorDisabled}
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
