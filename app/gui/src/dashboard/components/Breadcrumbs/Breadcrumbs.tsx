/**
 * @file Breadcrumbs component implementation.
 */

import ArrowRight from '#/assets/expand_arrow_right.svg'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import { createLeafComponent } from '@react-aria/collections'
import { Fragment, type ReactElement } from 'react'
import * as aria from 'react-aria-components'
import flattenChildren from 'react-keyed-flatten-children'
import { Button, type TestIdProps } from '../AriaComponents'
import { Icon } from '../Icon'
import { BreadcrumbCollapsedItem, BreadcrumbItem } from './BreadcrumbItem'
import { getItemsWithCollapsedItem, isCollapsedItem } from './utilities'

export const BREADCRUMBS_STYLES = tv({
  base: 'flex items-center gap-2 w-full',
  slots: {
    separator: 'text-primary last:hidden',
  },
})

/**
 * Props for {@link Breadcrumbs}
 */
export interface BreadcrumbsProps<T>
  extends aria.BreadcrumbsProps<T>,
    VariantProps<typeof BREADCRUMBS_STYLES>,
    TestIdProps {}

/**
 * A breadcrumb navigation component.
 */
export function Breadcrumbs<T extends object>(props: BreadcrumbsProps<T>) {
  const { children, items, className, variants = BREADCRUMBS_STYLES, testId } = props

  const styles = variants()

  if (items != null && typeof children === 'function') {
    return (
      <Button.Group gap="none" buttonVariants={{ variant: 'icon' }}>
        <BreadcrumbsItemsCollection<T> {...props} items={items} children={children} />
      </Button.Group>
    )
  }

  const itemsWithCollapsedItem = getItemsWithCollapsedItem<ReactElement>(flattenChildren(children))

  return (
    <Button.Group gap="none" buttonVariants={{ variant: 'icon' }}>
      <aria.Breadcrumbs {...props} className={styles.base({ className })} data-testid={testId}>
        {itemsWithCollapsedItem.map((item, index) => {
          const isLastItem = index === itemsWithCollapsedItem.length - 1

          const element =
            isCollapsedItem(item) ?
              <BreadcrumbCollapsedItem
                key="collapsed-item"
                items={item.items}
                children={(menuItem) => menuItem}
              />
            : item

          return (
            <>
              {element}
              {!isLastItem ?
                <BreadcrumbSeparator className={styles.separator()} />
              : null}
            </>
          )
        })}
      </aria.Breadcrumbs>
    </Button.Group>
  )
}

/**
 * Props for {@link BreadcrumbsItemsCollection}
 */
interface BreadcrumbsCollectionProps<T>
  extends aria.BreadcrumbsProps<T>,
    VariantProps<typeof BREADCRUMBS_STYLES>,
    TestIdProps {
  /** The children to render */
  readonly children: (item: T) => React.ReactNode
  /** The items to render */
  readonly items: Iterable<T>
}

/**
 * A lazy collection of breadcrumb items.
 */
function BreadcrumbsItemsCollection<T extends object>(props: BreadcrumbsCollectionProps<T>) {
  const { items, children, variants = BREADCRUMBS_STYLES, className } = props

  const styles = variants()

  const itemsWithCollapsedItem = getItemsWithCollapsedItem(items)

  return (
    <aria.Breadcrumbs
      {...props}
      items={itemsWithCollapsedItem}
      className={styles.base({ className })}
    >
      {(item) => {
        const separator = <BreadcrumbSeparator className={styles.separator()} />
        if (isCollapsedItem(item)) {
          return (
            <Fragment key="collapsed-item">
              <BreadcrumbCollapsedItem id={item.id} items={item.items} children={children} />
              {separator}
            </Fragment>
          )
        }

        return (
          <>
            {children(item)}
            {separator}
          </>
        )
      }}
    </aria.Breadcrumbs>
  )
}

/**
 * Props for {@link BreadcrumbSeparator}
 */
interface BreadcrumbSeparatorProps {
  readonly icon: string
  readonly className?: string
}

/**
 * A separator between breadcrumb items.
 */
// eslint-disable-next-line no-restricted-syntax
const BreadcrumbSeparator = createLeafComponent(
  'BreadcrumbSeparator',
  function BreadcrumbSeparator(props: BreadcrumbSeparatorProps) {
    const { icon = ArrowRight, className } = props

    return <Icon className={className}>{icon}</Icon>
  },
)

Breadcrumbs.Item = BreadcrumbItem
