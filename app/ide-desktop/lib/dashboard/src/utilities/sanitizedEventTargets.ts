/** @file Various common {@link EventTarget}s, without the unsafe catch-all overload
 * that accepts a `string` event name. */

/** `document` without the unsafe {@link EventTarget} overloads. */
interface SanitizedDocumentEventTarget
  extends Omit<Document, 'addEventListener' | 'removeEventListener'> {
  readonly addEventListener: <K extends keyof DocumentEventMap>(
    type: K,
    listener: (this: Document, ev: DocumentEventMap[K]) => unknown,
    options?: AddEventListenerOptions | boolean
  ) => void
  readonly removeEventListener: <K extends keyof DocumentEventMap>(
    type: K,
    listener: (this: Document, ev: DocumentEventMap[K]) => unknown,
    options?: EventListenerOptions | boolean
  ) => void
}

// This MUST be lowercase, so that it looks identical to the regular `document.`
// eslint-disable-next-line no-restricted-syntax
export const document: SanitizedDocumentEventTarget = globalThis.document

/** `document` without the unsafe {@link EventTarget} overloads. */
interface SanitizedWindowEventTarget
  extends Omit<Window, 'addEventListener' | 'removeEventListener'> {
  readonly addEventListener: <K extends keyof WindowEventMap>(
    type: K,
    listener: (this: Document, ev: WindowEventMap[K]) => unknown,
    options?: AddEventListenerOptions | boolean
  ) => void
  readonly removeEventListener: <K extends keyof WindowEventMap>(
    type: K,
    listener: (this: Document, ev: WindowEventMap[K]) => unknown,
    options?: EventListenerOptions | boolean
  ) => void
}

// This MUST be lowercase, so that it looks identical to the regular `document.`
// eslint-disable-next-line no-restricted-syntax
export const window: SanitizedWindowEventTarget = globalThis.window
