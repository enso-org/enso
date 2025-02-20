/**
 * Yields elements matching the given CSS selector, out of the hierarchy of elements from the given element to the root.
 */
export function* selectorHierarchy(element: Element, selectors: string) {
  for (;;) {
    const match = element.closest(selectors)
    if (!match) return
    yield match
    if (!match.parentElement) return
    element = match.parentElement
  }
}

/** Yield the input, if it is an {@link Element}, followed by each of its `parentElement`s. */
export function* elementHierarchy(
  el: Element | EventTarget | Node | null,
): IterableIterator<Element> {
  if (!(el instanceof Element) && el instanceof Node) {
    el = el.parentElement
  }
  while (el instanceof Element) {
    yield el
    el = el.parentElement
  }
}

/** TODO: Add docs */
export function dataAttribute<T extends string = string>(
  element: Element,
  key: string,
): T | undefined {
  return element instanceof HTMLElement && key in element.dataset ?
      (element.dataset[key] as T)
    : undefined
}
