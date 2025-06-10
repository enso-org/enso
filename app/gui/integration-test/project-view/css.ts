import type { ElementHandle } from 'playwright'

/**
 * Returns text content of the element, including CSS ::before and ::after content in the element's tree.
 * Currently whitespace produced around pseudo-elements is unspecified; block/inline logic is not implemented.
 */
export function computedContent(element: ElementHandle<HTMLElement | SVGElement>): Promise<string> {
  return element.evaluate<string>((element) => {
    const getPseudoElement = (element: HTMLElement | SVGElement, pseudo: string) => {
      const value = window.getComputedStyle(element, `::${pseudo}`).getPropertyValue('content')
      if (value.match(/['"]/)) return value.replace(/['"]/g, '')
    }
    const recurse = (element: HTMLElement | SVGElement): string => {
      return [
        getPseudoElement(element, 'before') ?? '',
        ...Array.from(element.childNodes, (child) =>
          child instanceof HTMLElement || child instanceof SVGElement ?
            recurse(child)
          : child.textContent,
        ),
        getPseudoElement(element, 'after') ?? '',
      ].join('')
    }
    return recurse(element)
  })
}
