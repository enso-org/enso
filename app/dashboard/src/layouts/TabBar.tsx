/** @file Switcher to choose the currently visible full-screen page. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'
import invariant from 'tiny-invariant'

import type * as text from 'enso-common/src/text'
import * as tabBar from 'enso-common/src/utilities/style/tabBar'

import * as projectHooks from '#/hooks/projectHooks'

import type { LaunchedProject } from '#/providers/ProjectsProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import StatelessSpinner, * as spinnerModule from '#/components/StatelessSpinner'
import FocusArea from '#/components/styled/FocusArea'
import SvgMask from '#/components/SvgMask'

import * as backend from '#/services/Backend'

import { useInputBindings } from '#/providers/InputBindingsProvider'
import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'
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
  readonly setSelectedTab: (element: HTMLElement) => void
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
export interface TabBarProps extends Readonly<React.PropsWithChildren> {
  readonly className?: string
}

/** Switcher to choose the currently visible full-screen page. */
export default function TabBar(props: TabBarProps) {
  const { children, className } = props
  const cleanupResizeObserverRef = React.useRef(() => {})
  const backgroundRef = React.useRef<HTMLDivElement | null>(null)
  const selectedTabRef = React.useRef<HTMLElement | null>(null)
  const [resizeObserver] = React.useState(
    () =>
      new ResizeObserver(() => {
        updateClipPath(selectedTabRef.current)
      }),
  )

  const [updateClipPath] = React.useState(() => {
    return (element: HTMLElement | null) => {
      const backgroundElement = backgroundRef.current
      if (backgroundElement) {
        const rootElement = backgroundElement.parentElement?.parentElement
        if (!element) {
          backgroundElement.style.clipPath = ''
          if (rootElement) {
            rootElement.style.clipPath = ''
          }
        } else {
          selectedTabRef.current = element
          const bounds = element.getBoundingClientRect()
          const rootBounds = backgroundElement.getBoundingClientRect()
          const { clipPath, rootClipPath } = tabBar.barClipPath(bounds, rootBounds, TAB_RADIUS_PX)
          backgroundElement.style.clipPath = clipPath
          if (rootElement) {
            rootElement.style.clipPath = rootClipPath
          }
        }
      }
    }
  })

  const setSelectedTab = React.useCallback(
    (element: HTMLElement | null) => {
      if (element) {
        updateClipPath(element)
        resizeObserver.observe(element)
        return () => {
          resizeObserver.unobserve(element)
        }
      } else {
        return
      }
    },
    [resizeObserver, updateClipPath],
  )

  const updateResizeObserver = (element: HTMLElement | null) => {
    cleanupResizeObserverRef.current()
    if (!(element instanceof HTMLElement)) {
      cleanupResizeObserverRef.current = () => {}
    } else {
      resizeObserver.observe(element)
      cleanupResizeObserverRef.current = () => {
        resizeObserver.unobserve(element)
      }
    }
  }

  return (
    <FocusArea direction="horizontal">
      {(innerProps) => (
        <div className={tailwindMerge.twMerge('relative flex grow', className)} {...innerProps}>
          <TabBarContext.Provider value={{ setSelectedTab }}>
            <aria.TabList className="flex h-12 shrink-0 grow transition-[clip-path] duration-300">
              <aria.Tab isDisabled>
                {/* Putting the background in a `Tab` is a hack, but it is required otherwise there
                 * are issues with the ref to the background being detached, resulting in the clip
                 * path cutout for the current tab not applying at all. */}
                <div
                  ref={(element) => {
                    backgroundRef.current = element
                    updateResizeObserver(element)
                  }}
                  className="pointer-events-none absolute inset-0 bg-primary/5 transition-[clip-path] duration-300"
                />
              </aria.Tab>
              {children}
            </aria.TabList>
          </TabBarContext.Provider>
        </div>
      )}
    </FocusArea>
  )
}

// ===========
// === Tab ===
// ===========

/** Props for a {@link Tab}. */
interface InternalTabProps extends Readonly<React.PropsWithChildren> {
  readonly 'data-testid'?: string
  readonly id: string
  readonly project?: LaunchedProject
  readonly isActive: boolean
  readonly isHidden?: boolean
  readonly icon: string | null
  readonly labelId: text.TextId
  readonly onClose?: () => void
  readonly onLoadEnd?: () => void
}

/** A tab in a {@link TabBar}. */
export function Tab(props: InternalTabProps) {
  const { id, project, isActive, isHidden = false, icon, labelId, children, onClose } = props
  const { onLoadEnd } = props
  const { getText } = textProvider.useText()
  const inputBindings = useInputBindings()
  const { setSelectedTab } = useTabBarContext()
  const ref = React.useRef<HTMLDivElement | null>(null)
  const isLoadingRef = React.useRef(true)
  const actuallyActive = isActive && !isHidden
  const [resizeObserver] = React.useState(
    () =>
      new ResizeObserver(() => {
        updateClipPath()
      }),
  )

  const [updateClipPath] = React.useState(() => {
    return () => {
      const element = ref.current
      if (element) {
        const bounds = element.getBoundingClientRect()
        element.style.clipPath = tabBar.tabClipPath(bounds, TAB_RADIUS_PX)
      }
    }
  })

  React.useEffect(() => {
    if (actuallyActive && onClose) {
      return inputBindings.attach(sanitizedEventTargets.document.body, 'keydown', {
        closeTab: onClose,
      })
    } else {
      return
    }
  }, [inputBindings, actuallyActive, onClose])

  React.useLayoutEffect(() => {
    if (actuallyActive && ref.current) {
      setSelectedTab(ref.current)
    }
  }, [actuallyActive, id, setSelectedTab])

  const { isLoading, data } = reactQuery.useQuery<backend.Project | null>(
    project?.id ?
      projectHooks.createGetProjectDetailsQuery.createPassiveListener(project.id)
    : { queryKey: ['__IGNORE__'], queryFn: reactQuery.skipToken },
  )

  const isFetching = isLoading || data == null || data.state.type !== backend.ProjectState.opened

  React.useEffect(() => {
    if (!isFetching && isLoadingRef.current) {
      isLoadingRef.current = false
      onLoadEnd?.()
    }
  }, [isFetching, onLoadEnd])

  return (
    <aria.Tab
      data-testid={props['data-testid']}
      ref={(element) => {
        if (element instanceof HTMLDivElement) {
          ref.current = element
          if (actuallyActive) {
            setSelectedTab(element)
          }
          resizeObserver.disconnect()
          resizeObserver.observe(element)
          updateClipPath()
        } else {
          ref.current = null
        }
      }}
      id={id}
      aria-label={getText(labelId)}
      className={tailwindMerge.twMerge(
        'relative -mx-6 flex h-full items-center gap-3 rounded-t-3xl px-10',
        !isActive &&
          'cursor-pointer opacity-50 hover:bg-frame hover:opacity-75 disabled:cursor-not-allowed disabled:opacity-30 [&.disabled]:cursor-not-allowed [&.disabled]:opacity-30',
        isHidden && 'hidden',
      )}
    >
      {icon != null &&
        (isLoading ?
          <StatelessSpinner
            state={spinnerModule.SpinnerState.loadingMedium}
            size={16}
            className={tailwindMerge.twMerge(onClose && 'group-hover:hidden focus-visible:hidden')}
          />
        : <SvgMask
            src={icon}
            className={tailwindMerge.twMerge(onClose && 'group-hover:hidden focus-visible:hidden')}
          />)}
      {data?.name ?? children}
      {onClose && (
        <div className="flex">
          <ariaComponents.CloseButton onPress={onClose} />
        </div>
      )}
    </aria.Tab>
  )
}
