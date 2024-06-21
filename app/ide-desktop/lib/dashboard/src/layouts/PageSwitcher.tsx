/** @file Switcher to choose the currently visible full-screen page. */
import * as React from 'react'

import DriveIcon from 'enso-assets/drive.svg'
import WorkspaceIcon from 'enso-assets/workspace.svg'

import type * as text from '#/text'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
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

/** The corner radius of the tabs. */
const TAB_RADIUS_PX = 24

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
  const visiblePageData = React.useMemo(
    () => PAGE_DATA.filter(pageData => (pageData.page !== Page.editor ? true : !isEditorDisabled)),
    [isEditorDisabled]
  )
  const cleanupResizeObserverRef = React.useRef(() => {})
  const backgroundRef = React.useRef<HTMLDivElement | null>(null)
  const selectedTabRef = React.useRef<HTMLDivElement | null>(null)
  const [resizeObserver] = React.useState(
    () =>
      new ResizeObserver(() => {
        updateClipPath(selectedTabRef.current)
      })
  )
  const [updateClipPath] = React.useState(() => {
    return (element: HTMLDivElement | null) => {
      const backgroundElement = backgroundRef.current
      if (backgroundElement != null) {
        selectedTabRef.current = element
        if (element == null) {
          backgroundElement.style.clipPath = ''
        } else {
          const bounds = element.getBoundingClientRect()
          const rootBounds = backgroundElement.getBoundingClientRect()
          const tabLeft = bounds.left - rootBounds.left
          const tabRight = bounds.right - rootBounds.left
          const segments = [
            'M 0 0',
            `L ${rootBounds.width} 0`,
            `L ${rootBounds.width} ${rootBounds.height}`,
            `L ${tabRight + TAB_RADIUS_PX} ${rootBounds.height}`,
            `A ${TAB_RADIUS_PX} ${TAB_RADIUS_PX} 0 0 1 ${tabRight} ${rootBounds.height - TAB_RADIUS_PX}`,
            `L ${tabRight} ${TAB_RADIUS_PX}`,
            `A ${TAB_RADIUS_PX} ${TAB_RADIUS_PX} 0 0 0 ${tabRight - TAB_RADIUS_PX} 0`,
            `L ${tabLeft + TAB_RADIUS_PX} 0`,
            `A ${TAB_RADIUS_PX} ${TAB_RADIUS_PX} 0 0 0 ${tabLeft} ${TAB_RADIUS_PX}`,
            `L ${tabLeft} ${rootBounds.height - TAB_RADIUS_PX}`,
            `A ${TAB_RADIUS_PX} ${TAB_RADIUS_PX} 0 0 1 ${tabLeft - TAB_RADIUS_PX} ${rootBounds.height}`,
            `L 0 ${rootBounds.height}`,
            'Z',
          ]
          backgroundElement.style.clipPath = `path("${segments.join(' ')}")`
        }
      }
    }
  })

  const updateResizeObserver = (element: HTMLElement | null) => {
    cleanupResizeObserverRef.current()
    if (element == null) {
      cleanupResizeObserverRef.current = () => {}
    } else {
      resizeObserver.observe(element)
      cleanupResizeObserverRef.current = () => {
        resizeObserver.unobserve(element)
      }
    }
  }

  React.useEffect(() => {
    if (visiblePageData.every(pageData => page !== pageData.page)) {
      updateClipPath(null)
    }
  }, [page, updateClipPath, visiblePageData])

  return (
    <div className="relative flex grow">
      <div
        ref={element => {
          backgroundRef.current = element
          updateResizeObserver(element)
        }}
        className="pointer-events-none absolute inset-0 bg-primary/5"
      />
      <Tabs>
        {visiblePageData.map(pageData => {
          const isActive = page === pageData.page
          return (
            <Tab
              key={pageData.page}
              ref={isActive ? updateClipPath : null}
              isActive={isActive}
              onPress={() => {
                setPage(pageData.page)
              }}
              {...pageData}
            />
          )
        })}
      </Tabs>
    </div>
  )
}

// ============
// === Tabs ===
// ============

/** Props for a {@link TabsInternal}. */
export interface InternalTabsProps extends Readonly<React.PropsWithChildren> {}

/** A tab list in a {@link PageSwitcher}. */
function TabsInternal(props: InternalTabsProps, ref: React.ForwardedRef<HTMLDivElement>) {
  const { children } = props
  return (
    <FocusArea direction="horizontal">
      {innerProps => (
        <div
          className="flex h-12 shrink-0 grow cursor-default items-center rounded-full"
          {...aria.mergeProps<React.JSX.IntrinsicElements['div']>()(innerProps, { ref })}
        >
          {children}
        </div>
      )}
    </FocusArea>
  )
}

const Tabs = React.forwardRef(TabsInternal)

// ===========
// === Tab ===
// ===========

/** Props for a {@link Tab}. */
interface InternalTabProps extends PageUIData {
  readonly isActive: boolean
  readonly onPress: () => void
}

/** A tab in a {@link PageSwitcher}. */
function TabInternal(props: InternalTabProps, ref: React.ForwardedRef<HTMLDivElement>) {
  const { isActive, page, nameId, icon, onPress } = props
  const { getText } = textProvider.useText()

  return (
    <div
      key={page}
      ref={ref}
      className={tailwindMerge.twMerge(
        'h-full transition-[padding-left]',
        page !== page && 'hover:enabled:bg-frame'
      )}
    >
      <ariaComponents.Button
        size="custom"
        variant="custom"
        icon={icon}
        isDisabled={isActive}
        isActive={isActive}
        className="flex h-full items-center gap-3 px-4"
        onPress={onPress}
      >
        {getText(nameId)}
      </ariaComponents.Button>
    </div>
  )
}

const Tab = React.forwardRef(TabInternal)
