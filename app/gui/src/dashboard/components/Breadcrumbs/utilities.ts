/**
 * @file Utilities for the Breadcrumbs component
 */
import type * as aria from 'react-aria-components'
import invariant from 'tiny-invariant'

const DEFAULT_START_VISIBLE_ITEMS_COUNT = 1
const DEFAULT_END_VISIBLE_ITEMS_COUNT = 2

const DROPDOWN_ITEM_BRAND = Symbol('DropdownItemBrand')

/**
 * A collapsed breadcrumb item.
 */
interface BreadcrumbCollapsedItem<T> {
  readonly [DROPDOWN_ITEM_BRAND]: true
  readonly items: T[]
  /**
   * Id determines a unique key across the collection
   */
  readonly id: aria.Key
}

/**
 * Options for {@link getItemsWithCollapsedItem}
 */
export interface GetItemsWithCollapsedItemOptions {
  /** The number of visible items */
  readonly startVisibleItemsCount?: number
  /** The number of last visible items */
  readonly endVisibleItemsCount?: number
}

/**
 * Get the items with a collapsed item.
 */
export function getItemsWithCollapsedItem<T>(
  items: Iterable<T>,
  options: GetItemsWithCollapsedItemOptions = {},
): Array<BreadcrumbCollapsedItem<T> | T> {
  const {
    startVisibleItemsCount = DEFAULT_START_VISIBLE_ITEMS_COUNT,
    endVisibleItemsCount = DEFAULT_END_VISIBLE_ITEMS_COUNT,
  } = options

  invariant(
    startVisibleItemsCount >= 0,
    'startVisibleItemsCount must be greater than or equal to 0',
  )
  invariant(endVisibleItemsCount >= 0, 'endVisibleItemsCount must be greater than or equal to 0')

  const totalVisibleItemsCount = startVisibleItemsCount + endVisibleItemsCount

  const itemsArray = Array.from(items)

  if (itemsArray.length <= totalVisibleItemsCount) {
    return itemsArray
  }

  const startVisibleItems = itemsArray.slice(0, startVisibleItemsCount)
  const endVisibleItems = endVisibleItemsCount === 0 ? [] : itemsArray.slice(-endVisibleItemsCount)

  const dropdownItem = {
    [DROPDOWN_ITEM_BRAND]: true,
    id: 'collapsed-item',
    items:
      endVisibleItemsCount === 0 ?
        itemsArray.slice(startVisibleItemsCount)
      : itemsArray.slice(startVisibleItemsCount, -endVisibleItemsCount),
  } satisfies BreadcrumbCollapsedItem<T>

  return [...startVisibleItems, dropdownItem, ...endVisibleItems]
}

/**
 * Check if an item is a collapsed item.
 */
export function isCollapsedItem<T>(item: unknown): item is BreadcrumbCollapsedItem<T> {
  return typeof item === 'object' && item != null && DROPDOWN_ITEM_BRAND in item
}
