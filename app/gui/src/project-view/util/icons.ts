import type { URLString } from '@/util/data/urlString'
import type { Icon } from '@/util/iconMetadata/iconName'

/**
 * String prefix used for embedded icon symbol's ID attributes.
 * Using prefix prevents accidental collision of global ID namespace.
 */
export const embeddedIconIdPrefix = 'svgicon:'

/** Get the SVG use `href` property value for given icon.  */
export function svgUseHref(icon: Icon | URLString): string {
  return icon.includes(':') ? icon : '#' + embeddedIconIdPrefix + icon
}

/**
 * Embed an SVG containing symbols into document body and transforms all detected ID attributes within the SVG body.
 */
export function embedSvgSymbols(
  svgSource: string,
  idTransform = (id: string) => embeddedIconIdPrefix + id,
): SVGSVGElement | null {
  const x = document.createElement('x')
  x.innerHTML = svgSource
  const svg = x.getElementsByTagName('svg')[0]
  if (svg) {
    svg.setAttribute('aria-hidden', 'true')
    Object.assign(svg.style, { position: 'absolute', width: '0', height: '0', overflow: 'hidden' })
    svg
      .querySelectorAll('[id]')
      .forEach((elem) => elem.setAttribute('id', idTransform(elem.getAttribute('id') ?? '')))
    document.body.insertBefore(svg, document.body.firstChild)
    return svg
  } else {
    console.error('Trying to embed SVG icons, but no SVG tag found.')
    return null
  }
}
