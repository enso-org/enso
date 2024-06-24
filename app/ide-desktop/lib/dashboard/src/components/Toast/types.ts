/**
 * @file
 *
 * This file contains the types for the Toast component.
 */
import type * as React from 'react'

import type * as sonner from 'sonner'

/**
 * The ID of a toast.
 */
export type Id = number | string

/**
 * The kind of toast to display.
 */
export type ToastKind = 'error' | 'info' | 'loading' | 'message' | 'success' | 'warning'

/**
 * An action to perform when a toast is interacted with.
 */
interface Action {
  readonly label: React.ReactNode | string
  readonly onPress: (id: Id, event: React.MouseEvent<HTMLButtonElement, MouseEvent>) => void
}

/**
 * A toast to display.
 */
type TToast = Omit<sonner.ToastT, 'action' | 'cancel' | 'id' | 'type'> & {
  readonly autoClose?: boolean
  readonly toastId: Id
  readonly type?: ToastKind
  readonly action?: Action | React.ReactNode
  readonly cancel?: Action | React.ReactNode
}

/**
 * The options for a toast.
 */
export type ToastOptions = Omit<TToast, 'delete' | 'jsx' | 'promise' | 'title' | 'toastId'> & {
  readonly toastId?: Id
}

/**
 * Promise toast option.
 */
export type ToastPromise<Data> = Promise<Data> | (() => Promise<Data>)

/**
 * Callback for a promise toast.
 */
type PromiseTResult<Data> =
  | React.ReactNode
  | string
  | ((data: Data) => Promise<React.ReactNode | string> | React.ReactNode | string)

/**
 * Promise toast options.
 */
export type TPromiseToast = Omit<ToastOptions, 'description'>

/**
 * Promise toast options.
 */
export type PromiseToastOptions<Data> = TPromiseToast & {
  readonly loading?: React.ReactNode | string
  readonly success?: PromiseTResult<Data>
  readonly error?: PromiseTResult<unknown>
  readonly description?: PromiseTResult<Data>
  readonly finally?: () => Promise<void> | void
}
