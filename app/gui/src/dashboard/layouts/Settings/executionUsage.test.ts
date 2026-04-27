/** @file Tests for `executionUsage.ts`. */
import * as v from 'vitest'
import { formatUptime } from './executionUsage'

v.test.each([
  [0, '0s'],
  [45, '45s'],
  [192, '3m 12s'],
  [4080, '1h 08m'],
  [7390, '2h 03m 10s'],
  [3600, '1h 00m'],
  [3610, '1h 00m 10s'],
])('formatExecutionUsageSeconds(%s)', (seconds, expected) => {
  v.expect(formatUptime(seconds)).toBe(expected)
})
