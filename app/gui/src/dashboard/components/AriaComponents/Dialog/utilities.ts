/** @file Utility functions for `Dialog`. */
import * as aria from '#/components/aria'
import { useIsLatestDialogStackItem } from '#/components/AriaComponents/Dialog/hooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useRef, type RefObject } from 'react'

const IGNORE_INTERACT_OUTSIDE_ELEMENTS = [
  // Toastify toasts
  '.Toastify__toast-container',
  // ReactQuery devtools
  '.tsqd-parent-container',
  // Our components that should ignore the interact outside event
  ':is(.enso-app, .enso-chat, .enso-portal-root) [data-ignore-click-outside]',
]

const IGNORE_INTERACT_OUTSIDE_ELEMENTS_SELECTOR = `:is(${IGNORE_INTERACT_OUTSIDE_ELEMENTS.join(', ')})`

/** Check if the element is a part of a component that should ignore the interact outside event */
export function shouldIgnoreInteractOutside(element: HTMLElement) {
  return element.closest(IGNORE_INTERACT_OUTSIDE_ELEMENTS_SELECTOR)
}

/** Props for {@link useInteractOutside} */
export interface UseInteractOutsideProps {
  readonly ref: RefObject<HTMLElement>
  readonly id: string
  readonly onInteractOutside?: (() => void) | null
  readonly isDisabled?: boolean
}

/** Hook that handles the interact outside event for the dialog */
export function useInteractOutside(props: UseInteractOutsideProps) {
  const { ref, id, onInteractOutside, isDisabled = false } = props
  const shouldCloseOnInteractOutsideRef = useRef(false)

  const isLatest = useIsLatestDialogStackItem(id)

  const onInteractOutsideStartCb = useEventCallback((e: MouseEvent) => {
    // eslint-disable-next-line no-restricted-syntax
    shouldCloseOnInteractOutsideRef.current = !shouldIgnoreInteractOutside(e.target as HTMLElement)
  })
  const onInteractOutsideCb = useEventCallback(() => {
    if (shouldCloseOnInteractOutsideRef.current) {
      onInteractOutside?.()
      shouldCloseOnInteractOutsideRef.current = false
    }
  })

  aria.useInteractOutside({
    ref,
    isDisabled: isDisabled || !isLatest,
    // We need to prevent the dialog from closing when interacting with the toastify container.
    // When an interaction starts, we check if the target is inside the toastify container,
    // and in the next callback we prevent the dialog from closing.
    // For some reason aria doesn't fire onInteractOutsideStart if onInteractOutside is not defined.
    onInteractOutsideStart: onInteractOutsideStartCb,
    onInteractOutside: onInteractOutsideCb,
  })
}

/** Animate the scale of the element. */
export function animateScale(element: HTMLElement, scale: number) {
  const duration = 200
  element.animate(
    [{ transform: 'scale(1)' }, { transform: `scale(${scale})` }, { transform: 'scale(1)' }],
    { duration, iterations: 1, direction: 'alternate' },
  )
}
