/**
 * Checks if the element's scroll size exceeds its client size.
 * @param element - The element to check
 * @returns Whether the element overflows.
 */
export function isOverflowing(element: HTMLElement | SVGElement): boolean {
  const { scrollHeight, scrollWidth, clientHeight, clientWidth } = element
  return scrollHeight > clientHeight || scrollWidth > clientWidth
}

/** Collects the `parentElement` hierarchy starting from `element`. */
export function elementHierarchy<T extends { parentElement: T | null }>(element: T | null): T[] {
  const elements: T[] = []
  while (element != null) {
    elements.push(element)
    element = element.parentElement
  }
  return elements
}
