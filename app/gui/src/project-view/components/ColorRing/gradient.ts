import { ensoColor, formatCssColor, normalizeHue } from '@/util/colors'
import * as iter from 'enso-common/src/utilities/data/iter'

export interface FixedRange {
  start: number
  end: number
  hue: number
  meetsPreviousRange: boolean
  meetsNextRange: boolean
}

/** Returns inputs sorted, deduplicated, with values near the end wrapped. */
function normalizeRangeInputs(inputs: Iterable<number>, radius: number) {
  const sortedInputs = [...inputs].sort((a, b) => a - b)
  const normalizedInputs = new Set<number>()
  const firstInput = sortedInputs[0]
  const lastInput = sortedInputs[sortedInputs.length - 1]
  if (lastInput != null && lastInput + radius > 1) normalizedInputs.add(lastInput - 1)
  sortedInputs.forEach((value) => normalizedInputs.add(value))
  if (firstInput != null && firstInput < radius) normalizedInputs.add(firstInput + 1)
  return normalizedInputs
}

/** TODO: Add docs */
export function seminormalizeHue(value: number) {
  return value === 1 ? 1 : normalizeHue(value)
}

/** TODO: Add docs */
export function rangesForInputs(inputs: Iterable<number>, radius: number): FixedRange[] {
  if (radius === 0) return []
  const ranges = new Array<FixedRange & { rawHue: number }>()
  const normalizedInputs = normalizeRangeInputs(inputs, radius)
  for (const hue of normalizedInputs) {
    const preferredStart = Math.max(hue - radius, 0)
    const preferredEnd = Math.min(hue + radius, 1)
    const prev = ranges[ranges.length - 1]
    if (prev && preferredStart < prev.end) {
      let midpoint = (prev.rawHue + hue) / 2
      if (midpoint >= 1) continue
      let meetsPreviousRange = true
      if (midpoint <= 0) {
        ranges.pop()
        midpoint = 0
        meetsPreviousRange = false
      } else {
        prev.end = midpoint
        prev.meetsNextRange = true
      }
      ranges.push({
        start: midpoint,
        end: preferredEnd,
        rawHue: hue,
        hue: seminormalizeHue(hue),
        meetsPreviousRange,
        meetsNextRange: false,
      })
    } else {
      const meetsPreviousRange = prev !== undefined && preferredStart < prev.end
      if (meetsPreviousRange) prev.meetsNextRange = true
      ranges.push({
        start: preferredStart,
        end: preferredEnd,
        rawHue: hue,
        hue: seminormalizeHue(hue),
        meetsPreviousRange,
        meetsNextRange: false,
      })
    }
  }
  const first = ranges[0]
  const last = ranges[ranges.length - 1]
  if (ranges.length >= 2 && first?.start === 0 && last?.end === 1) {
    first.meetsPreviousRange = true
    last.meetsNextRange = true
  }
  return ranges
}

export interface GradientPoint {
  hue: number
  angle: number
  angle2?: number
}
/** TODO: Add docs */
export function cssAngularColorStop({ hue, angle, angle2 }: GradientPoint) {
  return [
    formatCssColor(ensoColor(hue)),
    `${angle}turn`,
    ...(angle2 != null ? [`${angle}turn`] : []),
  ].join(' ')
}

/** TODO: Add docs */
export function gradientPoints(
  inputRanges: Iterable<FixedRange>,
  minStops?: number | undefined,
): GradientPoint[] {
  const points = new Array<GradientPoint>()
  const interpolationPoint = (angle: number) => ({ hue: angle, angle })
  const fixedRangeIter = new iter.Resumable(inputRanges)
  const min = Math.max(3, Math.round(minStops ?? 0))
  for (let i = 0; i < min; i++) {
    const angle = i / (min - 1)
    fixedRangeIter.advanceWhile((range) => range.end < angle)
    const nextFixedRange = fixedRangeIter.peek()
    if (!nextFixedRange || nextFixedRange.start > angle) points.push(interpolationPoint(angle))
  }
  for (const { start, end, hue, meetsPreviousRange, meetsNextRange } of inputRanges) {
    if (!meetsPreviousRange) points.push(interpolationPoint(start))
    points.push({ hue, angle: start, angle2: end })
    if (!meetsNextRange) points.push(interpolationPoint(end))
  }
  points.sort((a, b) => a.angle - b.angle)
  return points
}
