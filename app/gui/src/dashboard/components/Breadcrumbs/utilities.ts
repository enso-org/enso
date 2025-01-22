/**
 * @file Utilities for the Breadcrumbs component
 */

const DEFAULT_VISIBLE_ITEMS_COUNT = 3
const DEFAULT_LAST_VISIBLE_ITEMS_COUNT = 2

const DROPDOWN_ITEM_BRAND = Symbol('DropdownItemBrand')

/**
 * A collapsed breadcrumb item.
 */
interface BreadcrumbCollapsedItem<T> {
  readonly [DROPDOWN_ITEM_BRAND]: true
  readonly items: T[]
}

/**
 * Options for {@link getItemsWithCollapsedItem}
 */
export interface GetItemsWithCollapsedItemOptions {
  /** The number of visible items */
  readonly visibleItemsCount?: number
  /** The number of last visible items */
  readonly lastVisibleItemsCount?: number
}

export function getItemsWithCollapsedItem<T>(
  items: Array<ItemWithIndex<T>>,
  options: GetItemsWithCollapsedItemOptions,
): Array<BreadcrumbCollapsedItem<ItemWithIndex<T>> | ItemWithIndex<T>>

/**
 * Get the items with a collapsed item.
 */
export function getItemsWithCollapsedItem<T>(
  items: Iterable<T>,
  options: GetItemsWithCollapsedItemOptions,
): Array<BreadcrumbCollapsedItem<T> | T>

/**
 * Get the items with a collapsed item.
 */
export function getItemsWithCollapsedItem<T>(
  items: Array<ItemWithIndex<T>> | Iterable<T>,
  options: GetItemsWithCollapsedItemOptions = {},
) {
  const {
    visibleItemsCount = DEFAULT_VISIBLE_ITEMS_COUNT,
    lastVisibleItemsCount = DEFAULT_LAST_VISIBLE_ITEMS_COUNT,
  } = options

  const itemsArray = Array.isArray(items) ? items : Array.from(items)

  if (itemsArray.length <= visibleItemsCount) {
    return itemsArray
  }

  // This is safe because we checked the length above
  // eslint-disable-next-line no-restricted-syntax
  const firstItem = itemsArray[0] as T
  const lastVisibleItems = itemsArray.slice(-lastVisibleItemsCount)

  const dropdownItem = {
    [DROPDOWN_ITEM_BRAND]: true,
    items: itemsArray.slice(1, -lastVisibleItemsCount),
  } satisfies BreadcrumbCollapsedItem<T>

  return [firstItem, dropdownItem, ...lastVisibleItems]
}

/**
 * Check if an item is a collapsed item.
 */
export function isCollapsedItem<T>(item: unknown): item is BreadcrumbCollapsedItem<T> {
  return typeof item === 'object' && item != null && DROPDOWN_ITEM_BRAND in item
}

/**
 * An item with an index.
 */
export interface ItemWithIndex<T> {
  readonly item: T
  readonly index: number
}

/**
 * Attach indices to items.
 */
export function attachIndiciesToItems<T>(items: Iterable<T>): ItemWithIndex<T>[] {
  return Array.from(items).map((item, index) => ({ item, index }))
}
