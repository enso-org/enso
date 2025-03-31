/** @file Tests for `dateTime.ts`. */
import * as v from 'vitest'

import { MINUTE_MS, toRfc3339 } from 'enso-common/src/utilities/data/dateTime'

/** Adds a UTC offset to a {@link Date}. Daylight savings-aware. */
function convertLocalToUTC(date: Date) {
  const offsetMins = date.getTimezoneOffset()
  return new Date(Number(date) - offsetMins * MINUTE_MS)
}

v.test.each([
  { date: new Date(0), string: '1970-01-01T00:00:00.000Z' },
  {
    date: convertLocalToUTC(new Date(2001, 1, 3)),
    string: '2001-02-03T00:00:00.000Z',
  },
])('Date and time serialization', ({ date, string }) => {
  v.expect(toRfc3339(date)).toBe(string)
})
