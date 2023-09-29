import { formatCss, formatRgb } from 'culori'
import { v3 as hashString } from 'murmurhash'

/* Generate a sRGB color value from the provided string. */
export function colorFromString(s: string) {
  const hash: number = hashString(s)
  // Split the 32-bit hash value into parts of 12, 10 and 10 bits.
  const part1: number = (hash >> 20) & 0xFFF;
  const part2: number = (hash >> 10) & 0x3FF;
  const part3: number = hash & 0x3FF;
  // Range values below can be adjusted if necessary, they were chosen arbitrarily.
  const chroma = mapInt32(part1, 0.05, 0.14, 12)
  const hue = mapInt32(part2, 0, 360, 10)
  const lightness = mapInt32(part3, 0.52, 0.57, 10)
  let color = {
    mode: 'oklch',
    l: lightness,
    c: chroma,
    h: hue,
  }
  return formatCss(color)
}

/* Convert CSS color to `rgb(...)`. */
export function rgbFallback(color: string): string {
  return formatRgb(color)
}

/* Map `bits`-wide unsigned integer to the range `[rangeStart, rangeEnd)`. */
function mapInt32(value: number, rangeStart: number, rangeEnd: number, bits: number) {
  const maxInt = 2 ** bits
  return (value / maxInt) * (rangeEnd - rangeStart) + rangeStart
}
