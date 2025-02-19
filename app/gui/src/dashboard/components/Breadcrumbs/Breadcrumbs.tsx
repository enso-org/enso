/**
 * @file Breadcrumbs component implementation.
 */
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
import { useBreadcrumbs, type AriaBreadcrumbsProps, type DragAndDropHooks } from '../aria'
import { Button, type IconProp, type TestIdProps } from '../AriaComponents'
import { Icon } from '../Icon'
import { BreadcrumbCollapsedItem, BreadcrumbItem, BreadcrumbItemProvider } from './BreadcrumbItem'
import { getItemsWithCollapsedItem, isCollapsedItem } from './utilities'

export const BREADCRUMBS_STYLES = tv({
  base: 'flex items-center w-full',
  slots: { separator: 'text-primary last:hidden w-2.5 h-2.5 mt-[0.5px]' },
})

/**
 * Props for {@link Breadcrumbs}
 */
export interface BreadcrumbsProps
  extends AriaBreadcrumbsProps,
    VariantProps<typeof BREADCRUMBS_STYLES>,
    TestIdProps {
  /** The breadcrumb items. */
  readonly children: ReactNode
  /** Called when an item is acted upon (usually selection via press). */
  readonly onAction?: (key: Key) => Promise<void> | void
  readonly className?: string
  readonly dragAndDropHooks?: DragAndDropHooks | undefined
}

/**
 * A breadcrumb navigation component.
 */
export function Breadcrumbs(props: BreadcrumbsProps) {
  const {
    children,
    className,
    variants = BREADCRUMBS_STYLES,
    testId,
    onAction = () => {},
    dragAndDropHooks,
    ...breadcrumbsProps
  } = props

  const styles = variants()

  const onActionStableCallback = useEventCallback(onAction)
  const itemsWithCollapsedItem = getItemsWithCollapsedItem<ReactElement>(flattenChildren(children))

  return (
    <Button.GroupProvider variant="icon">
      <BreadcrumbInner {...breadcrumbsProps} className={styles.base({ className })} testId={testId}>
        {itemsWithCollapsedItem.map((item, i, array) => {
          const element =
            isCollapsedItem(item) ?
              <BreadcrumbCollapsedItem
                key="collapsed-item"
                items={item.items}
                children={(menuItem) => menuItem}
              />
            : item

          return (
            <Fragment key={element.key}>
              <BreadcrumbItemProvider
                isCurrent={i === array.length - 1}
                onAction={onActionStableCallback}
                onActionSpecified={props.onAction != null}
              >
                {element}
              </BreadcrumbItemProvider>

              <BreadcrumbSeparator className={styles.separator()} />
            </Fragment>
          )
        })}
      </BreadcrumbInner>
    </Button.GroupProvider>
  )
}

/**
 * Props for {@link BreadcrumbInner}
 */
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

/**
 * Props for {@link BreadcrumbSeparator}
 */
interface BreadcrumbSeparatorProps<Icon extends string> {
  readonly icon?: IconProp<Icon, never>
  readonly className?: string
}

/**
 * A separator between breadcrumb items.
 */
// eslint-disable-next-line no-restricted-syntax
const BreadcrumbSeparator = memo(function BreadcrumbSeparator<Icon extends string>(
  props: BreadcrumbSeparatorProps<Icon>,
) {
  const { icon = 'arrow_right', className } = props

  return <Icon className={className}>{icon}</Icon>
}) as <Icon extends string>(props: BreadcrumbSeparatorProps<Icon>) => ReactElement

Breadcrumbs.Item = BreadcrumbItem
