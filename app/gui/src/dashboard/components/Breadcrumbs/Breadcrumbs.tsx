/** @file A breadcrumb nagivation component. */
import { Button } from '#/components/Button'
import type { IconProp, TestIdProps } from '#/components/types'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import {
  Fragment,
  memo,
  type Key,
  type PropsWithChildren,
  type ReactElement,
  type ReactNode,
} from 'react'
import flattenChildren from 'react-keyed-flatten-children'
import { useBreadcrumbs, type AriaBreadcrumbsProps, type DropEvent } from '../aria'
import { Icon } from '../Icon'
import { BreadcrumbItem, BreadcrumbItemProvider } from './BreadcrumbItem'

// eslint-disable-next-line react-refresh/only-export-components
export const BREADCRUMBS_STYLES = tv({
  base: 'flex items-center w-full',
  slots: { separator: 'text-primary last:hidden w-2.5 h-2.5 mt-[0.5px]' },
})

/** The type of the `onDrop` callback. */
export type OnDrop = (key: Key, e: DropEvent) => Promise<void> | void

/** Props for {@link Breadcrumbs}. */
export interface BreadcrumbsProps
  extends AriaBreadcrumbsProps,
    VariantProps<typeof BREADCRUMBS_STYLES>,
    TestIdProps {
  /** The breadcrumb items. */
  readonly children: ReactNode
  /** Called when an item is acted upon (usually selection via press). */
  readonly onAction?: (key: Key) => Promise<void> | void
  readonly className?: string
  readonly onDrop?: OnDrop
}

/** A breadcrumb navigation component. */
export function Breadcrumbs(props: BreadcrumbsProps) {
  const {
    children,
    className,
    variants = BREADCRUMBS_STYLES,
    testId,
    onAction = () => {},
    onDrop = () => {},
    ...breadcrumbsProps
  } = props

  const styles = variants()

  const onActionStableCallback = useEventCallback(onAction)
  const onDropStableCallback = useEventCallback(onDrop)

  return (
    <Button.GroupProvider variant="icon">
      <BreadcrumbInner {...breadcrumbsProps} className={styles.base({ className })} testId={testId}>
        {flattenChildren(children).map((item: ReactElement, i: number, array: ReactElement[]) => (
          <Fragment key={item.key}>
            <BreadcrumbItemProvider
              isCurrent={i === array.length - 1}
              onAction={onActionStableCallback}
              onActionSpecified={props.onAction != null}
              onDrop={onDropStableCallback}
              onDropSpecified={props.onDrop != null}
            >
              {item}
            </BreadcrumbItemProvider>

            <BreadcrumbSeparator className={styles.separator()} />
          </Fragment>
        ))}
      </BreadcrumbInner>
    </Button.GroupProvider>
  )
}

/** Props for {@link BreadcrumbInner}. */
interface BreadcrumbInnerProps extends TestIdProps, AriaBreadcrumbsProps, PropsWithChildren {
  readonly className?: string
}

/**
 * Internal component for rendering the breadcrumbs.
 * @internal
 */
function BreadcrumbInner(props: BreadcrumbInnerProps) {
  const { children, className, testId } = props

  const { navProps } = useBreadcrumbs(props)

  return (
    <ol {...navProps} className={className} data-testid={testId}>
      {children}
    </ol>
  )
}

/** Props for {@link BreadcrumbSeparator}. */
interface BreadcrumbSeparatorProps<Icon extends string> {
  readonly icon?: IconProp<Icon, never>
  readonly className?: string
}

/** A separator between breadcrumb items. */
// eslint-disable-next-line no-restricted-syntax
const BreadcrumbSeparator = memo(function BreadcrumbSeparator<Icon extends string>(
  props: BreadcrumbSeparatorProps<Icon>,
) {
  const { icon = 'folder_closed', className } = props

  return <Icon className={className} icon={icon} />
}) as <Icon extends string>(props: BreadcrumbSeparatorProps<Icon>) => ReactElement

Breadcrumbs.Item = BreadcrumbItem
