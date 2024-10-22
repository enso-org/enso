import * as v from 'vitest'

import * as tabBar from '../tabBar'

interface TabClipPathInput {
  bounds: { width: number; height: number }
  radius: number
  side?: 'top' | 'right'
}

const dashboardTabCases = [
  {
    input: {
      bounds: { width: 164.2109375, height: 48 },
      radius: 24,
    },
    expected:
      'path("M 0 48 A 24 24 0 0 0 24 24 L 24 24 A 24 24 0 0 1 48 0 L 116.2109375 0 A 24 24 0 0 1 140.2109375 24 L 140.2109375 24 A 24 24 0 0 0 164.2109375 48 M 0 0")',
  },
  {
    input: {
      bounds: { width: 209.6171875, height: 48 },
      radius: 24,
    },
    expected:
      'path("M 0 48 A 24 24 0 0 0 24 24 L 24 24 A 24 24 0 0 1 48 0 L 161.6171875 0 A 24 24 0 0 1 185.6171875 24 L 185.6171875 24 A 24 24 0 0 0 209.6171875 48 M 0 0")',
  },
]

const guiTabCases = [
  {
    input: {
      bounds: { width: 44, height: 48 },
      radius: 8,
      side: 'right',
    },
    expected:
      'path("M 0 0 A 8 8 0 0 0 8 8 L 36 8 A 8 8 0 0 1 44 16 L 44 32 A 8 8 0 0 1 36 40 L 8 40 A 8 8 0 0 0 0 48 M 44 0")',
  },
]

v.test.each([
  { group: 'Dashboard', cases: dashboardTabCases },
  { group: 'GUI', cases: guiTabCases },
])('Tab clip path: $group', ({ cases }) => {
  cases.forEach(({ input, expected }) => {
    const result = tabBar.tabClipPath(input.bounds, input.radius, (input as TabClipPathInput)?.side)
    v.expect(result).toBe(expected)
  })
})
