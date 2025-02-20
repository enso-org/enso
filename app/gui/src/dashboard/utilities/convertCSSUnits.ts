/** @file Utility functions to convert between units. */
// Taken from https://stackoverflow.com/a/75178110.

const DUMMY_RECT = document.createElementNS('http://www.w3.org/2000/svg', 'rect')
const WIDTH = DUMMY_RECT.width.baseVal
const MODES = {
  // eslint-disable-next-line @typescript-eslint/naming-convention
  '%': WIDTH.SVG_LENGTHTYPE_PERCENTAGE,
  em: WIDTH.SVG_LENGTHTYPE_EMS,
  ex: WIDTH.SVG_LENGTHTYPE_EXS,
  px: WIDTH.SVG_LENGTHTYPE_PX,
  cm: WIDTH.SVG_LENGTHTYPE_CM,
  mm: WIDTH.SVG_LENGTHTYPE_MM,
  in: WIDTH.SVG_LENGTHTYPE_IN,
  pt: WIDTH.SVG_LENGTHTYPE_PT,
  pc: WIDTH.SVG_LENGTHTYPE_PC,
}

/** CSS units supported by {@link convertCSSUnits}. */
export type CSSUnit = keyof typeof MODES

/** Convert a CSS length measurement from one unit to another. */
export function convertCSSUnits(
  value: number,
  from: CSSUnit,
  to: CSSUnit,
  parent?: HTMLElement | SVGElement,
) {
  if (parent) {
    parent.appendChild(DUMMY_RECT)
  }
  WIDTH.newValueSpecifiedUnits(MODES[from], value)
  WIDTH.convertToSpecifiedUnits(MODES[to])
  const out = { number: WIDTH.valueInSpecifiedUnits, string: WIDTH.valueAsString }
  if (parent) {
    parent.removeChild(DUMMY_RECT)
  }
  return out
}

/** Convert a CSS length measurement from a CSS string value to a number in the given unit. */
export function convertCSSUnitString(
  value: string,
  to: CSSUnit,
  parent?: HTMLElement | SVGElement,
) {
  const match = value.match(/^([\d.]+)(%|rem|em|ex|px|cm|mm|in|pt|pc)$/)
  if (match) {
    const [, numericValueAsString = '', from = ''] = match
    const numericValue = Number(numericValueAsString)
    switch (from) {
      case 'rem': {
        // `rem` is just `em` from the root.
        return convertCSSUnits(numericValue, 'em', to)
      }
      default: {
        // This is SAFE, as the regex ensures that the only valid values are CSS units.
        // eslint-disable-next-line no-restricted-syntax
        return convertCSSUnits(numericValue, from as CSSUnit, to, parent)
      }
    }
  } else {
    return { number: 0, string: `0${to}` }
  }
}
