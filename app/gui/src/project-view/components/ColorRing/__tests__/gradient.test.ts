import { normalizeHue } from '@/util/colors'
import { fc, test as fcTest } from '@fast-check/vitest'
import { expect } from 'vitest'
import type { FixedRange, GradientPoint } from '../gradient'
import { gradientPoints, rangesForInputs } from '../gradient'

/** Check value ranges and internal consistency. */
function validateRange({ start, end }: FixedRange) {
  expect(start).toBeGreaterThanOrEqual(0)
  expect(start).toBeLessThan(1)
  expect(end).toBeGreaterThan(0)
  expect(end).toBeLessThanOrEqual(1)
  expect(end).toBeGreaterThan(start)
}

/** Check value ranges and internal consistency. */
function validateGradientPoint({ hue, angle, angle2 }: GradientPoint) {
  expect(hue).toBeGreaterThanOrEqual(0)
  expect(hue).toBeLessThanOrEqual(1)
  expect(angle).toBeGreaterThanOrEqual(0)
  expect(angle).toBeLessThanOrEqual(1)
  if (angle2 != null) {
    expect(angle2).toBeGreaterThanOrEqual(0)
    expect(angle2).toBeLessThanOrEqual(1)
    expect(angle).toBeLessThanOrEqual(angle2)
  } else {
    expect(hue).toBe(angle)
  }
}

interface AngularStop {
  hue: number
  angle: number
}
function angularStops(points: Iterable<GradientPoint>) {
  const stops = new Array<AngularStop>()
  for (const { hue, angle, angle2 } of points) {
    const normalized = normalizeHue(hue)
    stops.push({ hue: normalized, angle })
    if (angle2 != null) stops.push({ hue: normalized, angle: angle2 })
  }
  return stops
}

function stopSpans(stops: Iterable<AngularStop>) {
  const spans = new Array<{ start: number; end: number; hue: number }>()
  let prev: AngularStop | undefined = undefined
  for (const stop of stops) {
    if (prev && stop.angle !== prev.angle) {
      expect(stop.angle).toBeGreaterThanOrEqual(prev.angle)
      if (stop.hue !== 0) expect(stop.hue).toBeGreaterThanOrEqual(prev.hue)
      if (stop.hue === prev.hue) {
        spans.push({ start: prev.angle, end: stop.angle, hue: stop.hue })
      } else {
        expect(stop.hue).toBe(normalizeHue(stop.angle))
        expect(prev.hue).toBe(prev.angle)
      }
    }
    prev = stop
  }
  const first = spans[0]
  const last = spans[spans.length - 1]
  if (spans.length >= 2 && first && last && normalizeHue(first.hue) === normalizeHue(last.hue)) {
    expect(first.start).toBe(0)
    expect(last.end).toBe(1)
    spans.pop()
    first.start = last.end
  }
  return spans
}

function testGradients({ hues, radius }: { hues: number[]; radius: number }) {
  const approximate = (n: number) => normalizeHue(Math.round(n * 2 ** 20) / 2 ** 20)
  const approximateHues = new Set(hues.map(approximate))
  const ranges = rangesForInputs(approximateHues, radius)
  ranges.forEach(validateRange)
  const points = gradientPoints(ranges)
  points.forEach(validateGradientPoint)
  const stops = angularStops(points)
  expect(stops[0]?.angle).toBe(0)
  expect(stops[stops.length - 1]?.angle).toBe(1)
  const spans = stopSpans(stops)
  for (const span of spans) {
    expect(approximateHues).toContain(approximate(span.hue))
    if (span.start < span.end) {
      expect(span.hue === 0 ? 1 : span.hue).toBeGreaterThan(span.start)
      expect(span.hue).toBeLessThanOrEqual(span.end)
      expect(span.end - span.start).toBeLessThan(radius * 2 + 0.0000001)
    } else {
      expect(span.hue > span.start || span.hue < span.end)
      expect(1 - span.start + span.end).toBeLessThan(radius * 2 + 0.0000001)
    }
  }
  expect(spans.length).toEqual(approximateHues.size)
}

fcTest.prop({
  hues: fc.array(fc.float({ min: 0, max: 1, noNaN: true, maxExcluded: true })),
  /* This parameter comes from configuration, so we don't need to test unrealistically small or large values that may
     have their own edge cases. */
  radius: fc.float({ min: Math.fround(0.01), max: 0.25, noNaN: true }),
})('CSS gradients', testGradients)
