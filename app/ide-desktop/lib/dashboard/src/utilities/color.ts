/** @file Functions related to CSS colors. */

// =============
// === Types ===
// =============

/** A color in the LCh colorspace. */
export interface LChColor {
  readonly lightness: number
  readonly chroma: number
  readonly hue: number
  readonly alpha?: number
}

// =================
// === Constants ===
// =================

/** A pre-selected list of colors to be used in color pickers. */
export const COLORS: readonly [LChColor, ...LChColor[]] = [
  /* eslint-disable @typescript-eslint/no-magic-numbers */
  // Red
  { lightness: 50, chroma: 66, hue: 7 },
  // Orange
  { lightness: 50, chroma: 66, hue: 34 },
  // Yellow
  { lightness: 50, chroma: 66, hue: 80 },
  // Turquoise
  { lightness: 50, chroma: 66, hue: 139 },
  // Teal
  { lightness: 50, chroma: 66, hue: 172 },
  // Blue
  { lightness: 50, chroma: 66, hue: 271 },
  // Lavender
  { lightness: 50, chroma: 66, hue: 295 },
  // Pink
  { lightness: 50, chroma: 66, hue: 332 },
  // Light blue
  { lightness: 50, chroma: 22, hue: 252 },
  // Dark blue
  { lightness: 22, chroma: 13, hue: 252 },
  /* eslint-enable @typescript-eslint/no-magic-numbers */
]

export const COLOR_STRING_TO_COLOR = new Map(
  COLORS.map(color => [lChColorToCssColor(color), color])
)

export const INITIAL_COLOR_COUNTS = new Map(COLORS.map(color => [lChColorToCssColor(color), 0]))

// ==========================
// === lChColorToCssColor ===
// ==========================

/** Converts a {@link LChColor} to a CSS color string. */
export function lChColorToCssColor(color: LChColor): string {
  return 'alpha' in color
    ? `lcha(${color.lightness}% ${color.chroma} ${color.hue} / ${color.alpha})`
    : `lch(${color.lightness}% ${color.chroma} ${color.hue})`
}

// ======================
// === leastUsedColor ===
// ======================

/** The color that is used for the least labels. Ties are broken by order. */
export function leastUsedColor(colors: Iterable<LChColor>) {
  const colorCounts = new Map(INITIAL_COLOR_COUNTS)
  for (const color of colors) {
    const colorString = lChColorToCssColor(color)
    colorCounts.set(colorString, (colorCounts.get(colorString) ?? 0) + 1)
  }
  const min = Math.min(...colorCounts.values())
  const [minColor] = [...colorCounts.entries()].find(kv => kv[1] === min) ?? []
  return minColor == null ? COLORS[0] : COLOR_STRING_TO_COLOR.get(minColor) ?? COLORS[0]
}
