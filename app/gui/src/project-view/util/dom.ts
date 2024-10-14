/** TODO: Add docs */
export function* elementHierarchy(element: Element, selectors: string) {
  for (;;) {
    const match = element.closest(selectors)
    if (!match) return
    yield match
    if (!match.parentElement) return
    element = match.parentElement
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
