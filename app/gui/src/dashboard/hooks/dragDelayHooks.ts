/** @file Hooks to trigger an action on drag delay. */
import type { DropEnterEvent, DropOptions } from '#/components/aria'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useUnmount } from '#/hooks/unmountHooks'
import type { DOMAttributes, DragEvent } from 'react'
import { useRef } from 'react'

/** The default delay, in milliseconds, before the drag action triggers. */
const DEFAULT_DELAY_MS = 2000

/** Options for {@link useDragDelayAction}. */
export interface DragDelayActionOptions {
  readonly delayMs?: number
}

/** A callback for {@link useDragDelayAction}. */
export type DragDelayCallback<T> = (event: DragEvent<T>) => void

/** Trigger an action on a native HTML drop target. */
export function useDragDelayAction<T>(
  callback: DragDelayCallback<T> | undefined,
  options: DragDelayActionOptions = {},
) {
  const { delayMs = DEFAULT_DELAY_MS } = options

  const handle = useRef<number | null>(null)

  const cancelPreviousCallback = useEventCallback(() => {
    if (handle.current != null) {
      clearTimeout(handle.current)
    }
    handle.current = null
  })

  useUnmount(cancelPreviousCallback)

  const onDragLeave = useEventCallback((event: DragEvent<T>) => {
    if (
      event.currentTarget instanceof HTMLElement &&
      event.relatedTarget instanceof HTMLElement &&
      event.currentTarget.contains(event.relatedTarget)
    ) {
      return
    }
    cancelPreviousCallback()
  })

  return {
    onDragEnter: useEventCallback((event: DragEvent<T>) => {
      if (
        event.currentTarget instanceof HTMLElement &&
        event.relatedTarget instanceof HTMLElement &&
        event.currentTarget.contains(event.relatedTarget)
      ) {
        return
      }
      cancelPreviousCallback()
      handle.current = window.setTimeout(() => {
        callback?.(event)
      }, delayMs)
    }),
    onDragLeave,
    onDrop: onDragLeave,
  } as const satisfies DOMAttributes<T>
}

/** A callback for {@link useAriaDragDelayAction}. */
export type AriaDragDelayCallback = (event: DropEnterEvent) => void

/** Trigger an action on a `react-aria-components` drop target. */
export function useAriaDragDelayAction(
  callback: AriaDragDelayCallback | undefined,
  options: DragDelayActionOptions = {},
) {
  const { delayMs = DEFAULT_DELAY_MS } = options

  const handle = useRef<number | null>(null)

  const cancelPreviousCallback = useEventCallback(() => {
    if (handle.current != null) {
      clearTimeout(handle.current)
    }
    handle.current = null
  })

  useUnmount(cancelPreviousCallback)

  return {
    onDropEnter: useEventCallback((event) => {
      cancelPreviousCallback()
      handle.current = window.setTimeout(() => {
        callback?.(event)
      }, delayMs)
    }),
    onDropExit: cancelPreviousCallback,
    onDrop: cancelPreviousCallback,
  } as const satisfies Omit<DropOptions, 'getDropOperationForPoint' | 'hasDropButton' | 'ref'>
}
