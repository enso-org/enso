/** @file Switcher to choose the currently visible full-screen page. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'
import invariant from 'tiny-invariant'

import type * as text from 'enso-common/src/text'

import * as projectHooks from '#/hooks/projectHooks'

import type { LaunchedProject } from '#/providers/ProjectsProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import StatelessSpinner, * as spinnerModule from '#/components/StatelessSpinner'
import FocusArea from '#/components/styled/FocusArea'
import SvgMask from '#/components/SvgMask'

import * as backend from '#/services/Backend'

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
export interface TabBarProps extends Readonly<React.PropsWithChildren> {}

/** Switcher to choose the currently visible full-screen page. */
export default function TabBar(props: TabBarProps) {
  const { children } = props
  const cleanupResizeObserverRef = React.useRef(() => {})
  const backgroundRef = React.useRef<HTMLDivElement | null>()
  const selectedTabRef = React.useRef<HTMLElement | null>(null)
  const [resizeObserver] = React.useState(
    () =>
      new ResizeObserver(() => {
        updateClipPath(selectedTabRef.current)
      })
  )

  const [updateClipPath] = React.useState(() => {
    return (element: HTMLElement | null) => {
      const backgroundElement = backgroundRef.current
      if (backgroundElement != null) {
        if (element == null) {
          backgroundElement.style.clipPath = ''
        } else {
          selectedTabRef.current = element
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
    [resizeObserver, updateClipPath]
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
    <div className="relative flex grow">
      <TabBarContext.Provider value={{ setSelectedTab }}>
        <FocusArea direction="horizontal">
          {innerProps => (
            <aria.TabList
              className="flex h-12 shrink-0 grow cursor-default items-center rounded-full"
              {...innerProps}
            >
              <aria.Tab isDisabled>
                {/* Putting the background in a `Tab` is a hack, but it is required otherwise there
                 * are issues with the ref to the background being detached, resulting in the clip
                 * path cutout for the current tab not applying at all. */}
                <div
                  ref={element => {
                    backgroundRef.current = element
                    updateResizeObserver(element)
                  }}
                  className="pointer-events-none absolute inset-0 bg-primary/5"
                />
              </aria.Tab>
              {children}
            </aria.TabList>
          )}
        </FocusArea>
      </TabBarContext.Provider>
    </div>
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
  readonly icon: string
  readonly labelId: text.TextId
  readonly onClose?: () => void
  readonly onLoadEnd?: () => void
}

/** A tab in a {@link TabBar}. */
export function Tab(props: InternalTabProps) {
  const { id, project, isActive, isHidden = false, icon, labelId, children, onClose } = props
  const { onLoadEnd } = props
  const { setSelectedTab } = useTabBarContext()
  const ref = React.useRef<HTMLDivElement | null>(null)
  const isLoadingRef = React.useRef(true)
  const { getText } = textProvider.useText()
  const actuallyActive = isActive && !isHidden

  React.useLayoutEffect(() => {
    if (actuallyActive && ref.current) {
      setSelectedTab(ref.current)
    }
  }, [actuallyActive, id, setSelectedTab])

  const { isLoading, data } = reactQuery.useQuery<backend.Project>(
    project?.id
      ? projectHooks.createGetProjectDetailsQuery.createPassiveListener(project.id)
      : { queryKey: ['__IGNORE__'], queryFn: reactQuery.skipToken }
  )

  const isFetching =
    (isLoading || (data && data.state.type !== backend.ProjectState.opened)) ?? false

  React.useEffect(() => {
    if (!isFetching && isLoadingRef.current) {
      isLoadingRef.current = false
      onLoadEnd?.()
    }
  }, [isFetching, onLoadEnd])

  return (
    <aria.Tab
      data-testid={props['data-testid']}
      ref={element => {
        ref.current = element
        if (actuallyActive && element) {
          setSelectedTab(element)
        }
      }}
      id={id}
      aria-label={getText(labelId)}
      className={tailwindMerge.twMerge(
        'relative flex h-full items-center gap-3 rounded-t-2xl px-4',
        !isActive &&
          'cursor-pointer opacity-50 hover:bg-frame hover:opacity-75 disabled:cursor-not-allowed disabled:opacity-30 [&.disabled]:cursor-not-allowed [&.disabled]:opacity-30',
        isHidden && 'hidden'
      )}
    >
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
      {data?.name ?? children}
      {onClose && (
        <div className="flex">
          <ariaComponents.CloseButton onPress={onClose} />
        </div>
      )}
    </aria.Tab>
  )
}
