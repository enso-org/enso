/** @file Tests for `dateTime.ts`. */
import * as v from 'vitest'

import * as dateTime from '../dateTime'

// =============
// === Tests ===
// =============

/* eslint-disable @typescript-eslint/no-magic-numbers */

/** The number of minutes in an hour. */
const HOUR_MIN = 60
/** The offset of local time */
const TIMEZONE_OFFSET_HOURS = new Date().getTimezoneOffset() / HOUR_MIN

v.test.each([
    { date: new Date(0), string: '1970-01-01T00:00:00.000Z', chatString: '01/01/1970 10:00 AM' },
    {
        date: new Date(2001, 1, 3, -TIMEZONE_OFFSET_HOURS),
        string: '2001-02-03T00:00:00.000Z',
        chatString: '03/02/2001 10:00 AM',
    },
])('Date and time serialization', ({ date, string, chatString }) => {
    v.expect(dateTime.toRfc3339(date)).toBe(string)
    v.expect(dateTime.formatDateTimeChatFriendly(date)).toBe(chatString)
})
