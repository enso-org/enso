import type { Opt } from '@/util/data/opt'

/**
 * Checks if the element's scroll size exceeds its client size.
 * @param element - The element to check
 * @returns Whether the element overflows.
 */
export function isOverflowing(element: HTMLElement | SVGElement): boolean {
  const { scrollHeight, scrollWidth, clientHeight, clientWidth } = element
  return scrollHeight > clientHeight || scrollWidth > clientWidth
}

// /** Collects the `parentElement` hierarchy starting from `element`. */
// export function elementHierarchy<T extends { parentElement: T | null }>(element: T | null): T[] {
//   const elements: T[] = []
//   while (element != null) {
//     elements.push(element)
//     element = element.parentElement
//   }
//   return elements
// }

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
export function* elementHierarchy<T extends { parentElement: T | null }>(
  el: T | null,
): IterableIterator<T> {
  // if (!(el instanceof Element) && el instanceof Node) {
  //   el = el.parentElement
  // }
  while (el != null) {
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

/** Wrap optional number into 'px' value, or not. */
export function optPx(value: Opt<number>) {
  return value != null ? `${value}px` : undefined
}
