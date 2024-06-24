/**
 * @file
 *
 * Toast utility functions.
 */
import type * as React from 'react'

import * as sonner from 'sonner'

import type * as types from './types'

const ID_LENGTH = 6
const BASE_36 = 36
const DEFAULT_DURATION = 5000

/**
 * Wrap the toast method to add the id to the data object
 * and improve action ergonomics.
 */
function wrap(kind: types.ToastKind) {
  return (message: React.ReactNode | string, data?: types.ToastOptions) => {
    const type = data?.type ?? kind
    return sonner.toast[type](message, mapData(data ?? {}))
  }
}

/**
 * Map the data object to the sonner toast object.
 */
function mapData<Data extends types.TPromiseToast>(data: Data): sonner.ExternalToast {
  const toastId = data.toastId ?? Math.random().toString(BASE_36).slice(2, ID_LENGTH)
  const duration = data.autoClose === false ? Infinity : data.duration ?? DEFAULT_DURATION

  const dataActionIsObject =
    data.action != null && typeof data.action === 'object' && 'onPress' in data.action
  const dataActionOnPress = dataActionIsObject ? data.action.onPress : null

  const dataCancelIsObject =
    data.cancel != null && typeof data.cancel === 'object' && 'onPress' in data.cancel
  const dataCancelOnPress = dataCancelIsObject ? data.cancel.onPress : null

  return {
    ...data,
    duration,
    id: toastId,
    action: dataActionIsObject
      ? { ...data.action, onClick: event => dataActionOnPress?.(toastId, event) }
      : undefined,
    cancel: dataCancelIsObject
      ? { ...data.cancel, onClick: event => dataCancelOnPress?.(toastId, event) }
      : data.cancel,
  } satisfies sonner.ExternalToast
}

// eslint-disable-next-line no-restricted-syntax
const toast = {
  error: wrap('error'),
  loading: wrap('loading'),
  success: wrap('success'),
  warning: wrap('warning'),
  info: wrap('info'),
  message: wrap('message'),
  // eslint-disable-next-line no-restricted-syntax
  dismiss: sonner.toast.dismiss as (id: types.Id) => void,
  promise: <Data>(promise: types.ToastPromise<Data>, data?: types.PromiseToastOptions<Data>) =>
    sonner.toast.promise(promise, mapData(data ?? {})),
} as const

export { toast }
