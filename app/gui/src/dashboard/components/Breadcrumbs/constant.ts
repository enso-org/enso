/** @file Constants for `Breadcrumbs`. */
import type { DropEvent } from '#/components/aria'
import { noop } from '#/utilities/functions'
import { Key, createContext } from 'react'

/** Context props for {@link BreadcrumbItemProvider}. */
export interface BreadcrumbItemContextType {
  readonly isCurrent: boolean
  /**
   * Workaround to have optimized `onAction` callback using `useEventCallback` hook.
   * And be able to check if `onAction` prop was specified and id is not.
   */
  readonly onActionSpecified: boolean
  readonly onAction: (key: Key) => Promise<void> | void
  /**
   * Workaround to have optimized `onDrop` callback using `useEventCallback` hook.
   * And be able to check if `onDrop` prop was specified and id is not.
   */
  readonly onDropSpecified: boolean
  readonly onDrop: (key: Key, e: DropEvent) => Promise<void> | void
}

/** Context for the breadcrumb item. */
export const BreadcrumbItemContext = createContext<BreadcrumbItemContextType>({
  isCurrent: false,
  onActionSpecified: false,
  onAction: noop,
  onDropSpecified: false,
  onDrop: noop,
})
