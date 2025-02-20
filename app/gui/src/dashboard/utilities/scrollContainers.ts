/**
 * @file Functions for working with scroll containers.
 */

/**
 * A type that represents an HTML or SVG element.
 */
export type HTMLOrSVGElement = HTMLElement | SVGElement

/**
 * Finds all scroll containers that have overflow set to 'auto' or 'scroll'
 * Uses Tailwind CSS classes if possible, otherwise falls back to inline overflow.
 * @param element - The element to start searching from
 * @returns An array of scroll containers
 */
export function findScrollContainers(element: HTMLOrSVGElement | null): HTMLOrSVGElement[] {
  /**
   * Recursively find scroll containers.
   * @param nextElement - The element to check
   * @returns An array of scroll containers
   */
  const recurse = (nextElement: HTMLOrSVGElement): HTMLOrSVGElement[] => {
    return [nextElement, ...findScrollContainers(nextElement.parentElement)]
  }

  if (!element || element === document.body) return []

  if (hasTailwindOverflowHidden(element)) {
    return findScrollContainers(element.parentElement)
  }

  if (hasInlineOverflowHidden(element)) {
    return findScrollContainers(element.parentElement)
  }

  if (hasTailwindOverflow(element)) {
    return recurse(element)
  }

  if (hasInlineOverflow(element)) {
    return recurse(element)
  }

  const { overflow, overflowX, overflowY } = window.getComputedStyle(element)

  if ([overflow, overflowX, overflowY].some((prop) => prop === 'auto' || prop === 'scroll')) {
    return recurse(element)
  }

  return findScrollContainers(element.parentElement)
}

/**
 * Finds all containers that possbly have overflow (when scrollWidth/scrollHeight > clientWidth/clientHeight)
 * @param element - The element to start searching from
 * @returns An array of containers that possbly have overflow
 */
export function findOverflowContainers(element: HTMLOrSVGElement | null): HTMLOrSVGElement[] {
  const result: HTMLOrSVGElement[] = []

  if (!element || element === document.body) return result

  if (hasPossibleOverflow(element)) {
    result.push(element)
  }

  return [...result, ...findOverflowContainers(element.parentElement)]
}

/**
 * Finds all scroll containers that have overflow set to 'auto' or 'scroll' using Tailwind CSS classes.
 * This is a more efficient way to find scroll containers than using the `getComputedStyle` method,
 * but works only if the element has Tailwind CSS classes applied to it.
 * @param element - The element to start searching from
 * @returns An array of scroll containers
 */
export function findScrollContainersUsingTailwind(
  element: HTMLOrSVGElement | null,
): HTMLOrSVGElement[] {
  const result: HTMLOrSVGElement[] = []

  if (!element || element === document.body) return result

  if (hasTailwindOverflow(element)) {
    result.push(element)
  }

  return [...result, ...findScrollContainersUsingTailwind(element.parentElement)]
}

/**
 * Finds all overflow containers using the `getComputedStyle` method.
 * @param element - The element to start searching from
 * @returns An array of overflow containers
 */
export function findOverflowContainersUsingComputedStyle(
  element: HTMLOrSVGElement | null,
): HTMLOrSVGElement[] {
  const result: HTMLOrSVGElement[] = []

  if (!element || element === document.body) return result

  if (hasComputedStyleOverflow(element)) {
    result.push(element)
  }

  return [...result, ...findOverflowContainersUsingComputedStyle(element.parentElement)]
}

/**
 * Checks if the element has overflow set to 'auto' or 'scroll' using the `getComputedStyle` method.
 * @param element - The element to check
 * @returns True if the element has overflow set to 'auto' or 'scroll', false otherwise
 */
function hasComputedStyleOverflow(element: HTMLOrSVGElement): boolean {
  const { overflow, overflowX, overflowY } = window.getComputedStyle(element)
  return [overflow, overflowX, overflowY].some((prop) => prop === 'auto' || prop === 'scroll')
}

/**
 * Checks if the element has inline overflow.
 * @param element - The element to check
 * @returns True if the element has inline overflow, false otherwise
 */
export function hasInlineOverflow(element: HTMLOrSVGElement): boolean {
  const { overflow, overflowX, overflowY } = element.style
  return (
    overflow === 'auto' ||
    overflow === 'scroll' ||
    overflowX === 'auto' ||
    overflowX === 'scroll' ||
    overflowY === 'auto' ||
    overflowY === 'scroll'
  )
}

/**
 * Checks if the element has Tailwind CSS classes that indicate overflow.
 * @param element - The element to check
 * @returns True if the element has Tailwind CSS classes that indicate overflow, false otherwise
 */
export function hasTailwindOverflow(element: HTMLOrSVGElement): boolean {
  return (
    element.classList.contains('overflow-auto') ||
    element.classList.contains('overflow-scroll') ||
    element.classList.contains('overflow-x-auto') ||
    element.classList.contains('overflow-y-auto') ||
    element.classList.contains('overflow-x-scroll') ||
    element.classList.contains('overflow-y-scroll')
  )
}

/**
 * Checks if the element has possible overflow, when scrollWidth/scrollHeight > clientWidth/clientHeight.
 * @param element - The element to check
 * @returns True if the element has possible overflow, false otherwise
 */
export function hasPossibleOverflow(element: HTMLOrSVGElement): boolean {
  const { scrollHeight, scrollWidth, clientHeight, clientWidth } = element
  return scrollHeight > clientHeight || scrollWidth > clientWidth
}

/**
 * Checks if the element has Tailwind CSS classes that indicate overflow hidden.
 * @param element - The element to check
 * @returns True if the element has Tailwind CSS classes that indicate overflow hidden, false otherwise
 */
export function hasTailwindOverflowHidden(element: HTMLOrSVGElement): boolean {
  return (
    element.classList.contains('overflow-hidden') ||
    element.classList.contains('overflow-x-hidden') ||
    element.classList.contains('overflow-y-hidden') ||
    element.classList.contains('overflow-x-clip') ||
    element.classList.contains('overflow-y-clip')
  )
}

/**
 * Checks if the element has inline overflow hidden.
 * @param element - The element to check
 * @returns True if the element has inline overflow hidden, false otherwise
 */
export function hasInlineOverflowHidden(element: HTMLOrSVGElement): boolean {
  const { overflow, overflowX, overflowY } = element.style
  return (
    overflow === 'hidden' ||
    overflowX === 'hidden' ||
    overflowY === 'hidden' ||
    overflow === 'clip' ||
    overflowX === 'clip' ||
    overflowY === 'clip'
  )
}
