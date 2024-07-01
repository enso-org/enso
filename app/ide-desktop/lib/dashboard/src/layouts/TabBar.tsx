/** @file Switcher to choose the currently visible full-screen page. */
import * as React from 'react'

import invariant from 'tiny-invariant'

import type * as text from '#/text'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import StatelessSpinner, * as spinnerModule from '#/components/StatelessSpinner'
import FocusArea from '#/components/styled/FocusArea'
import SvgMask from '#/components/SvgMask'

import * as tailwindMerge from '#/utilities/tailwindMerge'

// =================
// === Constants ===
// =================

/** The corner radius of the tabs. */
const TAB_RADIUS_PX = 24

// =====================
// === TabBarContext ===
// =====================

let i = 0

/** Context for a {@link TabBarContext}. */
interface TabBarContextValue {
  readonly onTabSelected: (element: HTMLDivElement | null) => void
  readonly observeElement: (element: HTMLElement) => () => void
}

const TabBarContext = React.createContext<TabBarContextValue | null>(null)

/** Custom hook to get tab bar context. */
function useTabBarContext() {
  const context = React.useContext(TabBarContext)
  invariant(context, '`useTabBarContext` must be used inside a `<TabBar />`')
  return context
}

// ==============
// === TabBar ===
// ==============

/** Props for a {@link TabBar}. */
export interface TabBarProps extends Readonly<React.PropsWithChildren> {}

/** Switcher to choose the currently visible full-screen page. */
export default function TabBar(props: TabBarProps) {
  const { children } = props
  const cleanupResizeObserverRef = React.useRef(() => {})
  const backgroundRef = React.useRef<HTMLDivElement | null>(null)
  const selectedTabRef = React.useRef<HTMLDivElement | null>(null)
  const [resizeObserver] = React.useState(
    () =>
      new ResizeObserver(() => {
        console.log('A', selectedTabRef.current)
        updateClipPath(selectedTabRef.current)
      })
  )
  const [updateClipPath] = React.useState(() => {
    return (element: HTMLDivElement | null) => {
      const backgroundElement = backgroundRef.current
      // console.log(':0', backgroundElement, element)
      if (backgroundElement != null) {
        selectedTabRef.current = element
        // console.log(selectedTabRef, element)
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

  const j = i++
  React.useEffect(() => {
    console.log(':D', j)
    return () => {
      console.log('D:', j)
    }
  }, [])

  const updateResizeObserver = (element: HTMLElement | null) => {
    cleanupResizeObserverRef.current()
    if (element == null) {
      cleanupResizeObserverRef.current = () => {}
    } else {
      console.log(':0', element, selectedTabRef.current, selectedTabRef)
      resizeObserver.observe(element)
      cleanupResizeObserverRef.current = () => {
        resizeObserver.unobserve(element)
      }
    }
  }

  return (
    <TabBarContext.Provider
      value={{
        onTabSelected: updateClipPath,
        observeElement: element => {
          resizeObserver.observe(element)
          return () => {
            resizeObserver.unobserve(element)
          }
        },
      }}
    >
      <div className="relative flex grow">
        <div
          ref={element => {
            backgroundRef.current = element
            updateResizeObserver(element)
          }}
          className="pointer-events-none absolute inset-0 bg-primary/5"
        />
        <TabList>{children}</TabList>
      </div>
    </TabBarContext.Provider>
  )
}

// ===============
// === TabList ===
// ===============

/** Props for a {@link TabList}. */
export interface TabListProps extends Readonly<React.PropsWithChildren> {}

/** A tab list in a {@link TabBar}. */
function TabList(props: TabListProps) {
  const { children } = props
  return (
    <FocusArea direction="horizontal">
      {innerProps => (
        <aria.TabList
          className="flex h-12 shrink-0 grow cursor-default items-center rounded-full"
          {...innerProps}
        >
          {children}
        </aria.TabList>
      )}
    </FocusArea>
  )
}

// ===========
// === Tab ===
// ===========

/** Props for a {@link Tab}. */
interface InternalTabProps extends Readonly<React.PropsWithChildren> {
  readonly id: string
  readonly isActive: boolean
  readonly isHidden?: boolean
  readonly icon: string
  readonly labelId: text.TextId
  /** When the promise is in flight, the tab icon will instead be a loading spinner. */
  readonly loadingPromise?: Promise<unknown>
  readonly onClose?: () => void
}

/** A tab in a {@link TabBar}. */
export function Tab(props: InternalTabProps) {
  const { id, isActive, isHidden = false, icon, labelId, loadingPromise, children, onClose } = props
  const { onTabSelected, observeElement } = useTabBarContext()
  // This must not be a `useRef` as `react-aria-components` does not create the DOM elements
  // immediately.
  const [element, ref] = React.useState<HTMLDivElement | null>(null)
  const { getText } = textProvider.useText()
  const [isLoading, setIsLoading] = React.useState(loadingPromise != null)

  React.useLayoutEffect(() => {
    if (isActive) {
      onTabSelected(element)
    }
  }, [isActive, element, onTabSelected])

  React.useEffect(() => {
    if (element) {
      return observeElement(element)
    } else {
      return () => {}
    }
  }, [element, observeElement])

  React.useEffect(() => {
    if (loadingPromise) {
      setIsLoading(true)
      const abortController = new AbortController()
      const onLoaded = () => {
        if (!abortController.signal.aborted) {
          setIsLoading(false)
        }
      }
      loadingPromise.then(onLoaded, onLoaded)
      return () => {
        abortController.abort()
      }
    } else {
      setIsLoading(false)
      return
    }
  }, [loadingPromise])

  return (
    <aria.Tab
      ref={ref}
      id={id}
      isDisabled={isActive}
      aria-label={getText(labelId)}
      className={tailwindMerge.twMerge(
        'group relative flex h-full items-center gap-3 rounded-t-2xl px-4',
        !isActive &&
          'cursor-pointer opacity-50 hover:bg-frame hover:opacity-75 disabled:cursor-not-allowed disabled:opacity-30 [&.disabled]:cursor-not-allowed [&.disabled]:opacity-30',
        isHidden && 'hidden'
      )}
    >
      {onClose && (
        <div className="mt-[1px] hidden h-4 w-4 items-center justify-center group-hover:flex focus-visible:flex">
          <ariaComponents.CloseButton onPress={onClose} />
        </div>
      )}
      {isLoading ? (
        <StatelessSpinner
          state={spinnerModule.SpinnerState.loadingMedium}
          size={16}
          className={tailwindMerge.twMerge(onClose && 'group-hover:hidden focus-visible:hidden')}
        />
      ) : (
        <SvgMask
          src={icon}
          className={tailwindMerge.twMerge(onClose && 'group-hover:hidden focus-visible:hidden')}
        />
      )}
      {children}
    </aria.Tab>
  )
}
