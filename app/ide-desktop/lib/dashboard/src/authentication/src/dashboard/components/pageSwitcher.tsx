/** @file Switcher to choose the currently visible full-screen page. */
import * as React from 'react'

import DriveIcon from 'enso-assets/drive.svg'
import HomeIcon from 'enso-assets/home.svg'
import NetworkIcon from 'enso-assets/network.svg'

// ====================
// === PageSwitcher ===
// ====================

/** Main content of the screen. Only one should be visible at a time. */
export enum Page {
    home = 'home',
    drive = 'drive',
    editor = 'editor',
}

/** Data describing how to display a button for a pageg. */
interface PageUIData {
    page: Page
    icon: string
}

const PAGE_DATA: PageUIData[] = [
    { page: Page.home, icon: HomeIcon },
    { page: Page.drive, icon: DriveIcon },
    { page: Page.editor, icon: NetworkIcon },
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
        <div className="flex gap-4">
            {PAGE_DATA.map(pageData => {
                const isDisabled =
                    pageData.page === Page.home ||
                    (pageData.page === Page.editor && isEditorDisabled)
                return (
                    <button
                        key={pageData.page}
                        disabled={page === pageData.page || isDisabled}
                        className={`${page === pageData.page ? '' : 'opacity-50'} ${
                            isDisabled ? 'cursor-not-allowed' : ''
                        }`}
                        {...(isDisabled
                            ? {
                                  title:
                                      pageData.page === Page.editor
                                          ? 'No project is currently open.'
                                          : 'Not implemented yet.',
                              }
                            : {})}
                        onClick={() => {
                            setPage(pageData.page)
                        }}
                    >
                        <img src={pageData.icon} />
                    </button>
                )
            })}
        </div>
    )
}
