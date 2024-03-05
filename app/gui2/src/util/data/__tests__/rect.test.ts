import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { fc, test } from '@fast-check/vitest'
import { expect } from 'vitest'

test.prop({
  rectX: fc.nat(),
  rectY: fc.nat(),
  width: fc.nat(),
  height: fc.nat(),
  x: fc.nat(),
  y: fc.nat(),
})('offsetToInclude', ({ rectX, rectY, width, height, x, y }) => {
  const rect = new Rect(new Vec2(rectX, rectY), new Vec2(width, height))
  const point = new Vec2(x, y)
  const offsetRect = rect.offsetToInclude(point)
  expect(
    offsetRect === undefined,
    '`offsetToInclude` returns `undefined` IFFI the original `Rect` contains the point.',
  ).toBe(rect.contains(point))
  if (offsetRect === undefined) return
  expect(
    offsetRect.size == rect.size,
    'The result of `offsetToInclude` is the same size as the input `Rect`.',
  )
  expect(offsetRect.contains(point), 'The result of `offsetToInclude` contains the point.')
  offsetRect.corners()
})
