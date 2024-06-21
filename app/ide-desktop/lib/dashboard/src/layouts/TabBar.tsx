/** @file Switcher to choose the currently visible full-screen page. */
import * as React from 'react'

import invariant from 'tiny-invariant'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import FocusArea from '#/components/styled/FocusArea'

import * as tailwindMerge from '#/utilities/tailwindMerge'

// =================
// === Constants ===
// =================

/** The corner radius of the tabs. */
const TAB_RADIUS_PX = 24

// =====================
// === TabBarContext ===
// =====================

/** Context for a {@link TabBarContext}. */
interface TabBarContextValue {
  readonly updateClipPath: (element: HTMLDivElement | null) => void
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

  return (
    <TabBarContext.Provider value={{ updateClipPath }}>
      <div className="relative flex grow">
        <div
          ref={element => {
            backgroundRef.current = element
            updateResizeObserver(element)
          }}
          className="pointer-events-none absolute inset-0 bg-primary/5"
        />
        <Tabs>{children}</Tabs>
      </div>
    </TabBarContext.Provider>
  )
}

// ============
// === Tabs ===
// ============

/** Props for a {@link TabsInternal}. */
export interface InternalTabsProps extends Readonly<React.PropsWithChildren> {}

/** A tab list in a {@link TabBar}. */
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
interface InternalTabProps extends Readonly<React.PropsWithChildren> {
  readonly isActive: boolean
  readonly icon: string
  readonly onPress: () => void
  readonly onClose?: () => void
}

/** A tab in a {@link TabBar}. */
export function Tab(props: InternalTabProps) {
  const { isActive, icon, children, onPress, onClose } = props
  const { updateClipPath } = useTabBarContext()

  return (
    <div
      ref={isActive ? updateClipPath : null}
      className={tailwindMerge.twMerge('relative h-full', !isActive && 'hover:enabled:bg-frame')}
    >
      <ariaComponents.Button
        size="custom"
        variant="custom"
        icon={icon}
        isDisabled={isActive}
        isActive={isActive}
        className={tailwindMerge.twMerge(
          'relative flex h-full items-center gap-3 px-4',
          onClose && 'pr-10'
        )}
        onPress={onPress}
      >
        {children}
      </ariaComponents.Button>
      {onClose && (
        <ariaComponents.CloseButton
          className="absolute right-4 top-1/2 -translate-y-1/2"
          onPress={onClose}
        />
      )}
    </div>
  )
}
