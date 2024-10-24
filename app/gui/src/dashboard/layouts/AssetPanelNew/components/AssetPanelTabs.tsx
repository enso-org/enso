/**
 * @file
 *
 * Tabs for the asset panel. Contains the visual state for the tabs and animations.
 */
import { AnimatedBackground } from '#/components/AnimatedBackground'
import { useVisualTooltip } from '#/components/AriaComponents'
import { ErrorBoundary } from '#/components/ErrorBoundary'
import { Suspense } from '#/components/Suspense'
import SvgMask from '#/components/SvgMask'
import type { Spring } from 'framer-motion'
import { AnimatePresence, motion } from 'framer-motion'
import { useRef } from 'react'
import type { TabListProps, TabPanelProps, TabProps } from 'react-aria-components'
import { Tab, TabList, TabPanel, Tabs, type TabsProps } from 'react-aria-components'

/**
 * Props for a {@link AssetPanelTabs}.
 */
export interface AssetPanelTabsProps extends TabsProps {}

/**
 * Display a set of tabs.
 */
export function AssetPanelTabs(props: AssetPanelTabsProps) {
  const { children } = props

  return <Tabs {...props}>{children}</Tabs>
}

/**
 * Props for a {@link AssetPanelTabList}.
 */
export interface AssetPanelTabListProps<T extends object> extends TabListProps<T> {}

/**
 * Display a list of tabs.
 */
export function AssetPanelTabList<T extends object>(props: AssetPanelTabListProps<T>) {
  return (
    <AnimatedBackground>
      <TabList {...props} />
    </AnimatedBackground>
  )
}

/**
 * Props for a {@link AssetPanelTab}.
 */
export interface AssetPanelTabProps extends TabProps {
  readonly id: string
  readonly icon: string
  readonly label: string
  readonly isExpanded: boolean
  readonly onPress?: () => void
}

/**
 * Display a tab.
 */
export function AssetPanelTab(props: AssetPanelTabProps) {
  const { id, icon, label, isExpanded } = props

  const tabRef = useRef<HTMLDivElement>(null)

  const { targetProps, tooltip } = useVisualTooltip({
    children: label,
    targetRef: tabRef,
    overlayPositionProps: { placement: 'left' },
  })

  return (
    <Tab ref={tabRef} id={id} aria-label={label} className="aspect-square w-full cursor-pointer">
      {({ isSelected }) => (
        <>
          <AnimatedBackground.Item
            isSelected={isSelected && isExpanded}
            className="h-full w-full rounded-2xl"
            underlayElement={
              <>
                <div className="h-full w-full rounded-r-2xl bg-background" />
                <div className="absolute -top-3 left-0 aspect-square w-3 [background:radial-gradient(circle_at_100%_0%,_transparent_70%,_var(--color-background)_70%)]" />
                <div className="absolute -bottom-3 left-0 aspect-square w-3 [background:radial-gradient(circle_at_100%_100%,_transparent_70%,_var(--color-background)_70%)]" />
              </>
            }
          >
            <motion.div
              className="h-full w-full"
              // eslint-disable-next-line @typescript-eslint/no-magic-numbers
              initial={{ x: 100 }}
              animate={{ x: 0 }}
              // eslint-disable-next-line @typescript-eslint/no-magic-numbers
              exit={{ x: 100 }}
              // eslint-disable-next-line @typescript-eslint/no-magic-numbers
              transition={DEFAULT_TRANSITION_OPTIONS}
            >
              <div
                ref={tabRef}
                className="flex h-full w-full items-center justify-center"
                {...targetProps}
              >
                <SvgMask src={icon} />
              </div>
            </motion.div>
          </AnimatedBackground.Item>

          {tooltip}
        </>
      )}
    </Tab>
  )
}

/**
 * Props for a {@link AssetPanelTabPanel}.
 */
export interface AssetPanelTabPanelProps extends TabPanelProps {}

const DEFAULT_TRANSITION_OPTIONS: Spring = {
  type: 'spring',
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  stiffness: 300,
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  damping: 40,
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  mass: 0.8,
}

/**
 * Display a tab panel.
 */
export function AssetPanelTabPanel(props: AssetPanelTabPanelProps) {
  const { children, id = '' } = props

  return (
    <TabPanel className="contents" shouldForceMount {...props}>
      {(renderProps) => (
        <AnimatePresence mode="popLayout">
          {renderProps.state.selectionManager.isSelected(id) && (
            <motion.div
              // eslint-disable-next-line @typescript-eslint/no-magic-numbers
              initial={{ x: 16, filter: 'blur(4px)', opacity: 0 }}
              animate={{ x: 0, filter: 'blur(0px)', opacity: 1 }}
              // eslint-disable-next-line @typescript-eslint/no-magic-numbers
              exit={{ x: 16, filter: 'blur(4px)', opacity: 0 }}
              transition={DEFAULT_TRANSITION_OPTIONS}
              className="flex h-full w-full flex-col overflow-y-auto scroll-offset-edge-3xl"
            >
              <Suspense loaderProps={{ className: 'my-auto' }}>
                <ErrorBoundary>
                  <div className="pointer-events-auto flex h-fit min-h-full w-full shrink-0 flex-col px-4 py-5">
                    {typeof children === 'function' ? children(renderProps) : children}
                  </div>
                </ErrorBoundary>
              </Suspense>
            </motion.div>
          )}
        </AnimatePresence>
      )}
    </TabPanel>
  )
}

AssetPanelTabs.Tab = AssetPanelTab
AssetPanelTabs.TabPanel = AssetPanelTabPanel
AssetPanelTabs.TabList = AssetPanelTabList
