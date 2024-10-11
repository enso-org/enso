import { converter, formatCss, formatRgb, modeOklch, modeRgb, useMode, type Oklch } from 'culori/fn'
import { v3 as hashString } from 'murmurhash'
export { type Oklch }

useMode(modeOklch)
useMode(modeRgb)

const oklch = converter('oklch')

/** Check if given css is supported in the browser. */
export function cssSupported(css: string): boolean {
  return typeof CSS !== 'undefined' && 'supports' in CSS && CSS.supports(css)
}

/** Whether the browser supports `oklch` colorspace. */
export const browserSupportsOklch: boolean = cssSupported('color: oklch(0 0 0)')

/** Generate a CSS color value from the provided string. */
export function colorFromString(s: string) {
  const hash: number = hashString(s)
  const hue = mapInt32(hash & 0x3ff, 0, 1, 10)
  return formatCssColor(ensoColor(hue))
}

/** Returns the enso color for a given hue, in the range 0-1. */
export function ensoColor(hue: number): Oklch {
  return {
    mode: 'oklch',
    l: 0.464,
    c: 0.14,
    h: normalizeHue(hue) * 360,
  }
}

/** Normalize a value to the range 0-1, as used for hues. */
export function normalizeHue(value: number) {
  return ((value % 1) + 1) % 1
}

/** Format an OKLCH color in CSS. */
export function formatCssColor(color: Oklch) {
  return browserSupportsOklch ? formatCss(color) : formatRgb(color)
}

/** Parse the input as a CSS color value; convert it to Oklch if it isn't already. */
export function parseCssColor(cssColor: string): Oklch | undefined {
  return oklch(cssColor)
}

/** Map `bits`-wide unsigned integer to the range `[rangeStart, rangeEnd)`. */
function mapInt32(value: number, rangeStart: number, rangeEnd: number, bits: number) {
  const maxInt = 2 ** bits
  return (value / maxInt) * (rangeEnd - rangeStart) + rangeStart
}
