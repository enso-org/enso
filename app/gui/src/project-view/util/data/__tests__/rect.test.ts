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
  const rect = Rect.XYWH(rectX, rectY, width, height)
  const point = new Vec2(x, y)
  const offsetRect = rect.offsetToInclude(point)
  expect(
    offsetRect === undefined,
    '`offsetToInclude` returns `undefined` iffi the original `Rect` contains the point.',
  ).toBe(rect.contains(point))
  if (offsetRect === undefined) return
  expect(
    offsetRect.size === rect.size,
    'The result of `offsetToInclude` is the same size as the input `Rect`.',
  )
  expect(offsetRect.contains(point), 'The result of `offsetToInclude` contains the point.')
  const dx = Math.max(0, rect.left - point.x, point.x - rect.right)
  const dy = Math.max(0, rect.top - point.y, point.y - rect.bottom)
  expect(
    Math.abs(offsetRect.left - rect.left),
    '`offsetToInclude` has shifted the `Rect` by the minimum distance that reaches the point on the x-axis.',
  ).toBe(dx)
  expect(
    Math.abs(offsetRect.top - rect.top),
    '`offsetToInclude` has shifted the `Rect` by the minimum distance that reaches the point on the y-axis.',
  ).toBe(dy)
})
