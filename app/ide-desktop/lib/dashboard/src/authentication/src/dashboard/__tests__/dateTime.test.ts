/** @file Tests for `dateTime.ts`. */
import * as v from 'vitest'

import * as dateTime from '../dateTime'

// =============
// === Tests ===
// =============

/* eslint-disable @typescript-eslint/no-magic-numbers */

/** The number of milliseconds in an hour. */
const HOUR_MS = 3_600_000
/** The number of minutes in an hour. */
const HOUR_MIN = 60
const TIMEZONE_OFFSET_MINS = new Date().getTimezoneOffset()
/** The offset of local time */
const TIMEZONE_OFFSET_HOURS = Math.floor(TIMEZONE_OFFSET_MINS / HOUR_MIN)

v.test.each([
    { date: new Date(0), string: '1970-01-01T00:00:00.000Z' },
    {
        date: new Date(2001, 1, 3, -TIMEZONE_OFFSET_HOURS),
        string: '2001-02-03T00:00:00.000Z',
    },
])('Date and time serialization', ({ date, string }) => {
    v.expect(dateTime.toRfc3339(date)).toBe(string)
})

v.test.each([
    { date: new Date(TIMEZONE_OFFSET_HOURS * HOUR_MS), chatString: `01/01/1970 00:00 AM` },
    {
        date: new Date(2001, 1, 3),
        chatString: `03/02/2001 00:00 AM`,
    },
])('Date and time serialization', ({ date, chatString }) => {
    v.expect(dateTime.formatDateTimeChatFriendly(date)).toBe(chatString)
})
