/** @file Switcher to choose the currently visible full-screen page. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import type * as text from 'enso-common/src/text'

import ErrorFilledIcon from '#/assets/warning.svg'
import * as projectHooks from '#/hooks/projectHooks'
import type { LaunchedProject } from '#/providers/ProjectsProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import { StatelessSpinner } from '#/components/StatelessSpinner'
import SvgMask from '#/components/SvgMask'

import { AnimatedBackground } from '#/components/AnimatedBackground'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useBackendForProjectType } from '#/providers/BackendProvider'
import { useInputBindings } from '#/providers/InputBindingsProvider'
import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'
import * as tailwindMerge from '#/utilities/tailwindMerge'
import { twJoin } from '#/utilities/tailwindMerge'
import { motion } from 'framer-motion'

/** Props for a {@link TabBar}. */
export interface TabBarProps<T extends object> extends aria.TabListProps<T> {
  readonly className?: string
}

/** Switcher to choose the currently visible full-screen page. */
export default function TabBar<T extends object>(props: TabBarProps<T>) {
  const { className, ...rest } = props

  const classes = React.useMemo(() => tailwindMerge.twJoin('flex grow', className), [className])

  return (
    <AnimatedBackground>
      <div className={classes}>
        <aria.TabList<T> className="flex h-12 shrink-0 grow px-2" {...rest} />
      </div>
    </AnimatedBackground>
  )
}

/** Props for a {@link Tab}. */
export interface TabProps extends Readonly<React.PropsWithChildren> {
  readonly 'data-testid'?: string
  readonly id: string
  readonly isActive: boolean
  readonly isHidden?: boolean
  readonly icon: React.ReactNode | string | null
  readonly labelId: text.TextId
  readonly onClose?: () => void
}

const UNDERLAY_ELEMENT = (
  <>
    <div className="h-full w-full rounded-t-4xl bg-dashboard" />
    <div className="absolute -left-5 bottom-0 aspect-square w-5 -rotate-90 [background:radial-gradient(circle_at_100%_0%,_transparent_70%,_var(--color-dashboard-background)_70%)]" />
    <div className="absolute -right-5 bottom-0 aspect-square w-5 -rotate-90 [background:radial-gradient(circle_at_100%_100%,_transparent_70%,_var(--color-dashboard-background)_70%)]" />
  </>
)

/** A tab in a {@link TabBar}. */
export function Tab(props: TabProps) {
  const { id, isActive, isHidden = false, icon, labelId, children, onClose } = props
  const { getText } = textProvider.useText()
  const inputBindings = useInputBindings()

  const stableOnClose = useEventCallback(() => {
    onClose?.()
  })

  React.useEffect(() => {
    if (isActive) {
      return inputBindings.attach(sanitizedEventTargets.document.body, 'keydown', {
        closeTab: stableOnClose,
      })
    }
  }, [inputBindings, isActive, stableOnClose])

  return (
    <aria.Tab
      data-testid={props['data-testid']}
      id={id}
      aria-label={getText(labelId)}
      className={tailwindMerge.twJoin(
        'disabled:cursor-not-allowed disabled:opacity-30 [&.disabled]:cursor-not-allowed [&.disabled]:opacity-30',
        !isActive && 'cursor-pointer',
        isHidden && 'hidden',
      )}
    >
      {({ isSelected, isHovered }) => (
        <AnimatedBackground.Item
          isSelected={isSelected}
          className="h-full w-full rounded-t-3xl px-4"
          underlayElement={UNDERLAY_ELEMENT}
        >
          <div
            className={twJoin(
              'relative flex h-full w-full items-center justify-center gap-3',
              isSelected || isHovered ? 'text-primary' : 'text-disabled',
            )}
          >
            <motion.div
              variants={{ active: { opacity: 1 }, inactive: { opacity: 0 } }}
              initial="inactive"
              animate={!isSelected && isHovered ? 'active' : 'inactive'}
              className="pointer-events-none absolute -inset-x-2.5 inset-y-2 -z-1 rounded-3xl bg-dashboard transition-colors duration-300"
            />
            {typeof icon === 'string' ?
              <SvgMask
                src={icon}
                className={tailwindMerge.twJoin(
                  onClose && 'group-hover:hidden focus-visible:hidden',
                )}
              />
            : icon}
            <ariaComponents.Text truncate="1" className="max-w-40" color="current">
              {children}
            </ariaComponents.Text>

            {onClose && <ariaComponents.CloseButton onPress={onClose} />}
          </div>
        </AnimatedBackground.Item>
      )}
    </aria.Tab>
  )
}

/** Props for a {@link ProjectTab}. */
export interface ProjectTabProps extends Omit<TabProps, 'onClose'> {
  readonly project: LaunchedProject
  readonly onLoadEnd?: (project: LaunchedProject) => void
  readonly onClose?: (project: LaunchedProject) => void
}

const SPINNER = <StatelessSpinner state="loading-medium" size={16} />

/** A {@link Tab} that displays the name of the project. */
export function ProjectTab(props: ProjectTabProps) {
  const { project, onLoadEnd, onClose, icon: iconRaw, ...rest } = props

  const didNotifyOnLoadEnd = React.useRef(false)
  const backend = useBackendForProjectType(project.type)

  const stableOnLoadEnd = useEventCallback(() => {
    onLoadEnd?.(project)
  })

  const stableOnClose = useEventCallback(() => {
    onClose?.(project)
  })

  const {
    data: isOpened,
    isSuccess,
    isError,
  } = reactQuery.useQuery({
    ...projectHooks.createGetProjectDetailsQuery({
      assetId: project.id,
      backend,
    }),
    select: (data) => projectHooks.OPENED_PROJECT_STATES.has(data.state.type),
  })

  const isReady = isSuccess && isOpened

  React.useEffect(() => {
    if (isReady && !didNotifyOnLoadEnd.current) {
      didNotifyOnLoadEnd.current = true
      stableOnLoadEnd()
    }
  }, [isReady, stableOnLoadEnd])

  React.useEffect(() => {
    if (!isReady) {
      didNotifyOnLoadEnd.current = false
    }
  }, [isReady])

  const icon = (() => {
    if (isReady) {
      return iconRaw
    }

    if (isError) {
      return <SvgMask src={ErrorFilledIcon} className="text-danger" />
    }

    return SPINNER
  })()

  return <Tab {...rest} icon={icon} onClose={stableOnClose} />
}

TabBar.ProjectTab = ProjectTab
TabBar.Tab = Tab
