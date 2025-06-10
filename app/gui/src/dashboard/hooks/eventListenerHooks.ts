/**
 * @file
 *
 * Set of hooks to work with native event listeners.
 */
import { IS_DEV_MODE } from 'enso-common/src/detect'
import type { RefObject } from 'react'
import { useEffect, useRef } from 'react'

import { useEventCallback } from './eventCallbackHooks'

/**
 * Options to pass to the event listener.
 */
export type UseEventListenerParams = Omit<AddEventListenerOptions, 'signal'> & {
  readonly isDisabled?: boolean
  readonly debug?: boolean
}

function useEventListener<K extends keyof WindowEventMap>(
  eventName: K,
  handler: (event: WindowEventMap[K]) => void,
  element: RefObject<Window> | Window,
  options?: UseEventListenerParams | boolean,
): void

function useEventListener<K extends keyof HTMLElementEventMap, T extends Element = HTMLDivElement>(
  eventName: K,
  handler: (event: HTMLElementEventMap[K]) => void,
  element: RefObject<T> | T,
  options?: UseEventListenerParams | boolean,
): void

function useEventListener<K extends keyof DocumentEventMap>(
  eventName: K,
  handler: (event: DocumentEventMap[K]) => void,
  element: Document | RefObject<Document>,
  options?: UseEventListenerParams | boolean,
): void

/**
 * Hook to attach an event listener to an element.
 * @param eventName - The name of the event to listen for.
 * @param handler - The handler to call when the event is triggered.
 * @param element - The element to add the event listener to.
 * @param options - The options to pass to the event listener.
 */
function useEventListener<
  KW extends keyof WindowEventMap,
  KH extends keyof HTMLElementEventMap,
  T extends HTMLElement = HTMLElement,
>(
  eventName: KH | KW,
  handler: (event: Event | HTMLElementEventMap[KH] | WindowEventMap[KW]) => void,
  element: RefObject<T> | T,
  options: UseEventListenerParams | boolean = { passive: true },
) {
  const {
    isDisabled = false,
    capture = false,
    once = false,
    passive = true,
    debug = false,
  } = typeof options === 'object' ? options : { passive: options }

  const startTime = useRef<number | null>(null)

  const handlerEvent = useEventCallback<typeof handler>((...args) => {
    if (debug && IS_DEV_MODE) {
      const currentTime = performance.now()

      const timeSlice = startTime.current == null ? 'N/A' : `${currentTime - startTime.current}ms`
      const timestamp = (() => {
        const date = new Date()
        return `${date.getHours()}:${date.getMinutes()}:${date.getSeconds()}:${date.getMilliseconds()}`
      })()

      /* eslint-disable no-restricted-properties */
      console.group(`Animation, Event: ${eventName}, timeStamp: ${timestamp}`)
      console.debug({
        timestamp,
        timeSlice,
        eventName,
        target: element,
        options: { passive, capture, once },
        ...args,
      })
      console.groupEnd()
      /* eslint-enable no-restricted-properties */

      startTime.current = performance.now()
    }

    handler(...args)
  })

  useEffect(() => {
    if (isDisabled) {
      return
    }

    const targetElement = elementIsRef(element) ? element.current : element

    if (targetElement != null && 'addEventListener' in targetElement) {
      targetElement.addEventListener(eventName, handlerEvent, { passive, capture, once })

      return () => {
        targetElement.removeEventListener(eventName, handlerEvent)
      }
    }
  }, [eventName, handlerEvent, element, isDisabled, passive, capture, once])
}

export { useEventListener }

/**
 * Check if the element is an SVGElement.
 */
function elementIsSVGElement(element: unknown): element is SVGElement {
  return element instanceof SVGElement
}

/**
 * Check if the element is an HTMLElement.
 */
function elementIsHTMLElement(element: unknown): element is HTMLElement {
  return element instanceof HTMLElement
}

/**
 * Check if the element is a RefObject.
 */
function elementIsRef(element: unknown): element is RefObject<Element> {
  if (elementIsDocument(element)) {
    return false
  }

  if (elementIsSVGElement(element)) {
    return false
  }

  if (elementIsHTMLElement(element)) {
    return false
  }

  if (elementIsWindow(element)) {
    return false
  }

  return typeof element === 'object' && element != null && 'current' in element
}

/**
 * Check if the element is a Document.
 */
function elementIsDocument(element: unknown): element is Document {
  return element instanceof Document
}

/**
 * Check if the element is a Window.
 */
function elementIsWindow(element: unknown): element is Window {
  return element === window
}
