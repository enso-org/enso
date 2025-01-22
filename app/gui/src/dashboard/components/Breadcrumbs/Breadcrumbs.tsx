/**
 * @file Breadcrumbs component implementation.
 */

import ArrowRight from '#/assets/expand_arrow_right.svg'
import TreeDotsOutline from '#/assets/three_horizontal_dots_outline.svg'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import { createLeafComponent } from '@react-aria/collections'
import { isValidElement, type ForwardedRef, type ReactElement } from 'react'
import * as aria from 'react-aria-components'
import flattenChildren from 'react-keyed-flatten-children'
import { Button, Menu, type TestIdProps } from '../AriaComponents'
import SvgMask from '../SvgMask'
import { attachIndiciesToItems, getItemsWithCollapsedItem, isCollapsedItem } from './utilities'

export const BREADCRUMBS_STYLES = tv({
  base: 'flex items-center gap-2 w-full',
  slots: {
    root: 'flex items-center gap-2 w-full',
    list: 'flex items-center gap-2 overflow-hidden',
    separator: 'text-gray-400',
    item: 'text-gray-600 hover:text-gray-900 transition-colors duration-200',
    itemDisabled: 'text-gray-400 cursor-not-allowed',
    itemCurrent: 'text-gray-900 font-medium',
    menu: 'bg-white border border-gray-200 rounded-md shadow-lg py-1 min-w-[200px] z-50',
    menuItem:
      'px-4 py-2 text-sm text-gray-700 hover:bg-gray-100 cursor-pointer transition-colors duration-200',
    menuItemDisabled: 'px-4 py-2 text-sm text-gray-400 cursor-not-allowed',
  },
})

export const BREADCRUMB_ITEM_STYLES = tv({
  base: 'flex items-center gap-2 text-gray-600 hover:text-gray-900 transition-colors duration-200',
  slots: {
    link: 'text-gray-600 hover:text-gray-900 transition-colors duration-200 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2 rounded-sm',
  },
  variants: {
    isDisabled: {
      true: {
        base: 'text-gray-400 cursor-not-allowed pointer-events-none',
        link: 'text-gray-400 cursor-not-allowed pointer-events-none',
      },
    },
    isCurrent: {
      true: {
        base: 'text-gray-900 font-medium',
        link: 'text-gray-900 font-medium hover:text-gray-900',
      },
    },
  },
})

/**
 * Props for {@link BreadcrumbItem}
 */
export interface BreadcrumbItemProps
  extends Omit<aria.BreadcrumbProps, 'id'>,
    Omit<aria.LinkProps, 'children' | 'className' | 'style'>,
    TestIdProps,
    VariantProps<typeof BREADCRUMB_ITEM_STYLES> {
  /** A unique id for the breadcrumb, which will be passed to `onAction` when the breadcrumb is pressed. */
  readonly id?: aria.Key | undefined
  /** An optional suffix element to render after the breadcrumb content */
  readonly suffix?: React.ReactNode
}

/**
 * Props for {@link Breadcrumbs}
 */
export interface BreadcrumbsProps<T>
  extends aria.BreadcrumbsProps<T>,
    VariantProps<typeof BREADCRUMBS_STYLES>,
    TestIdProps {}

/**
 * A single breadcrumb item.
 */
export function BreadcrumbItem(props: BreadcrumbItemProps) {
  const {
    children,
    id,
    variants = BREADCRUMB_ITEM_STYLES,
    className,
    style = {},
    suffix,
    ...linkProps
  } = props

  const styles = variants()

  return (
    <aria.Breadcrumb
      className={(renderProps) =>
        styles.base({
          className: typeof className === 'function' ? className(renderProps) : className,
        })
      }
      style={style}
      {...(id != null ? { id } : {})}
    >
      {(renderProps) => (
        <div className="flex items-center gap-2">
          <aria.Link {...linkProps} className={styles.link()}>
            {typeof children === 'function' ? children(renderProps) : children}
          </aria.Link>
          {suffix}
        </div>
      )}
    </aria.Breadcrumb>
  )
}

/**
 * A breadcrumb navigation component.
 */
export function Breadcrumbs<T extends object>(props: BreadcrumbsProps<T>) {
  const { children, items, className, variants = BREADCRUMBS_STYLES, testId } = props

  const styles = variants()

  if (items != null && typeof children === 'function') {
    return <BreadcrumbsItemsCollection<T> {...props} items={items} children={children} />
  }

  const ttms = flattenChildren(children)
  const itemsWithCollapsedItem = getItemsWithCollapsedItem<ReactElement>(ttms)

  return (
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
            {!isLastItem && <BreadcrumbSeparator />}
          </>
        )
      })}
    </aria.Breadcrumbs>
  )
}

/**
 * Props for {@link BreadcrumbsItemsCollection}
 */
interface BreadcrumbsCollectionProps<T> {
  /** The children to render */
  readonly children: (item: T) => React.ReactNode
  /** The items to render */
  readonly items: Iterable<T>
}

/**
 * A lazy collection of breadcrumb items.
 */
function BreadcrumbsItemsCollection<T extends object>(props: BreadcrumbsCollectionProps<T>) {
  const { items, children } = props

  const itemsWithIndex = attachIndiciesToItems(items)
  const itemsWithCollapsedItem = getItemsWithCollapsedItem(itemsWithIndex)

  return (
    <aria.Breadcrumbs {...props} items={itemsWithCollapsedItem}>
      {(item) => {
        if (isCollapsedItem(item)) {
          return <BreadcrumbCollapsedItem items={item.items} children={children} />
        }

        return children(item.item)
      }}
    </aria.Breadcrumbs>
  )
}

/**
 * Props for {@link BreadcrumbCollapsedItem}
 */
interface BreadcrumbCollapsedItemProps<T extends object> {
  /** The items to render */
  readonly items: T[]
  /** The children to render */
  readonly children: (item: T) => React.ReactNode
}

/**
 * A collapsed breadcrumb item.
 */
// eslint-disable-next-line no-restricted-syntax
const BreadcrumbCollapsedItem = createLeafComponent(
  'BreadcrumbCollapsedItem',
  function BreadcrumbCollapsedItem<T extends object>(props: BreadcrumbCollapsedItemProps<T>) {
    const { items, children } = props

    return (
      <Menu.Trigger>
        <Button icon={TreeDotsOutline} variant="icon" />

        <Menu items={items}>
          {(menuItem) => {
            const breadcrumb = children(menuItem)

            if (isValidElement(breadcrumb) && breadcrumb.type === BreadcrumbItem) {
              // eslint-disable-next-line no-restricted-syntax
              const { testId, id = 'dropdown-item' } = breadcrumb.props as BreadcrumbItemProps

              return (
                <Menu.Item testId={testId} id={id}>
                  {breadcrumb}
                </Menu.Item>
              )
            }

            return null
          }}
        </Menu>
      </Menu.Trigger>
    )
  },
) as <T extends object>(props: BreadcrumbCollapsedItemProps<T>) => React.ReactNode

/**
 * Props for {@link BreadcrumbSeparator}
 */
interface BreadcrumbSeparatorProps {
  readonly icon: string
}

/**
 * A separator between breadcrumb items.
 */
// eslint-disable-next-line no-restricted-syntax
const BreadcrumbSeparator = createLeafComponent(
  'BreadcrumbSeparator',
  function BreadcrumbSeparator(props: BreadcrumbSeparatorProps, ref: ForwardedRef<HTMLDivElement>) {
    const { icon = ArrowRight } = props
    return <SvgMask ref={ref} src={icon} />
  },
)

Breadcrumbs.Item = BreadcrumbItem
