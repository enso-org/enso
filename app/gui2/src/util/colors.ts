import { formatCss, inGamut, toGamut } from 'culori'
import { v3 as hashString } from 'murmurhash'

const inOklch = inGamut('rgb')
const toOklch = toGamut('rgb')

/* Generate a sRGB color value from the provided string. */
export function colorFromString(s: string) {
  const hash: number = hashString(s)
  // Range values below can be adjusted if necessary, they were chosen arbitrarily.
  const hue = mapInt32(hash, 0, 360)
  const chroma = mapInt32(hash, 0.05, 0.14)
  const lightness = mapInt32(hash, 0.52, 0.57)
  let color = {
    mode: 'oklch',
    l: lightness,
    c: chroma,
    h: hue,
  }
  if (!inOklch(color)) {
    color = toOklch(color)
  }
  return formatCss(color)
}

/* Map the 32-bit signed integer to the range `[rangeStart, rangeEnd)`. */
function mapInt32(value: number, rangeStart: number, rangeEnd: number) {
  const minInt = -(2 ** 31)
  const maxInt = 2 ** 31 - 1
  return ((value - minInt) / (maxInt - minInt)) * (rangeEnd - rangeStart) + rangeStart
}
