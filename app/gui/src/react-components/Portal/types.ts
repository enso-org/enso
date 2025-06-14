/**
 * @file
 * Types for the Portal component
 */
import type * as React from 'react'

/** The props for the Portal component */
export interface PortalProps {
  /**
   * Ref, where `<Portal />` should render its children
   * By default it renders under `<Root />`
   * @default null
   */
  readonly root?: React.MutableRefObject<HTMLElement | null> | React.RefObject<HTMLElement | null>
  /**
   * Disables portal's API
   * @default false
   */
  readonly isDisabled?: boolean
  /** Callback, will be called after portal's children mounted */
  readonly onMount?: () => void
  readonly children?: React.ReactNode
}
