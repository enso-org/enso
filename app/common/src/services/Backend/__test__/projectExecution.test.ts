import { CalendarDateTime, ZonedDateTime, toZoned } from '@internationalized/date'
import * as v from 'vitest'
import { IanaTimeZone, toRfc3339 } from '../../../utilities/data/dateTime'
import {
  ProjectId,
  type ProjectExecutionInfo,
  type ProjectExecutionRepeatInfo,
} from '../../Backend'
import {
  firstProjectExecutionOnOrAfter,
  getProjectExecutionRepetitionsForDateRange,
  nextProjectExecutionDate,
} from '../projectExecution'

const TIME_ZONE = 'America/Los_Angeles'

/** A date in {@link TIME_ZONE}. */
function date(year: number, month: number, day: number, hour = 0, minute = 0) {
  return toZoned(new CalendarDateTime(year, month, day, hour, minute), TIME_ZONE)
}

/** A project execution with the given repeat, starting on 2000-01-01 at 07:03. */
function execution(repeat: ProjectExecutionRepeatInfo): ProjectExecutionInfo {
  return {
    projectId: ProjectId('project-aaaaaaaa'),
    repeat,
    startDate: toRfc3339(date(2000, 1, 1, 7, 3).toDate()),
    endDate: null,
    timeZone: IanaTimeZone('UTC'),
    maxDurationMinutes: 60,
    parallelMode: 'ignore',
    tag: undefined,
  }
}

// Days of the week are numbered from Monday (`0`) to Sunday (`6`), as returned by `getDay`.
const WEEKLY_EXECUTION = execution({ type: 'weekly', daysOfWeek: [4, 6] })
// Months are zero-based: `2` is March and `5` is June.
const MONTHLY_DATE_EXECUTION = execution({ type: 'monthlyDate', date: 15, months: [2, 5] })
const APRIL_DATE_EXECUTION = execution({ type: 'monthlyDate', date: 15, months: [3] })
// The Monday of the second week (days 8 to 14) of the month.
const MONTHLY_WEEKDAY_EXECUTION = execution({
  type: 'monthlyWeekday',
  weekNumber: 2,
  dayOfWeek: 0,
  months: [3, 4],
})
const APRIL_WEEKDAY_EXECUTION = execution({
  type: 'monthlyWeekday',
  weekNumber: 2,
  dayOfWeek: 0,
  months: [3],
})

v.test.each([
  {
    info: WEEKLY_EXECUTION,
    current: date(2000, 6, 4, 7, 3),
    next1: date(2000, 6, 9, 7, 3),
    next2: date(2000, 6, 11, 7, 3),
    next3: date(2000, 6, 16, 7, 3),
  },
  {
    info: MONTHLY_DATE_EXECUTION,
    current: date(2000, 3, 15, 7, 3),
    next1: date(2000, 6, 15, 7, 3),
    next2: date(2001, 3, 15, 7, 3),
    next3: date(2001, 6, 15, 7, 3),
  },
  {
    info: APRIL_DATE_EXECUTION,
    current: date(2000, 4, 15, 7, 3),
    next1: date(2001, 4, 15, 7, 3),
    next2: date(2002, 4, 15, 7, 3),
    next3: date(2003, 4, 15, 7, 3),
  },
  {
    info: MONTHLY_WEEKDAY_EXECUTION,
    current: date(2000, 4, 10, 7, 3),
    // 2000-05-08 is itself a Monday, so it is the Monday of the second week.
    next1: date(2000, 5, 8, 7, 3),
    next2: date(2001, 4, 9, 7, 3),
    next3: date(2001, 5, 14, 7, 3),
  },
  {
    info: APRIL_WEEKDAY_EXECUTION,
    current: date(2000, 4, 10, 7, 3),
    next1: date(2001, 4, 9, 7, 3),
    next2: date(2002, 4, 8, 7, 3),
    next3: date(2003, 4, 14, 7, 3),
  },
] satisfies readonly {
  info: ProjectExecutionInfo
  current: ZonedDateTime
  next1: ZonedDateTime
  next2: ZonedDateTime
  next3: ZonedDateTime
}[])(
  'Get next project execution date ($info.repeat.type, current: $current)',
  ({ info, current, next1, next2, next3 }) => {
    v.expect(nextProjectExecutionDate(info, current)?.toString()).toBe(next1.toString())
    v.expect(nextProjectExecutionDate(info, next1)?.toString()).toBe(next2.toString())
    v.expect(nextProjectExecutionDate(info, next2)?.toString()).toBe(next3.toString())
  },
)

v.test.each([
  { info: WEEKLY_EXECUTION, current: date(1999, 1, 1), next: date(2000, 1, 2, 7, 3) },
  { info: WEEKLY_EXECUTION, current: date(2000, 11, 16), next: date(2000, 11, 17, 7, 3) },
  { info: MONTHLY_DATE_EXECUTION, current: date(2000, 3, 10), next: date(2000, 3, 15, 7, 3) },
  { info: MONTHLY_DATE_EXECUTION, current: date(2000, 3, 20), next: date(2000, 6, 15, 7, 3) },
  { info: MONTHLY_DATE_EXECUTION, current: date(2000, 7, 1), next: date(2001, 3, 15, 7, 3) },
  { info: APRIL_DATE_EXECUTION, current: date(2000, 3, 20), next: date(2000, 4, 15, 7, 3) },
  { info: APRIL_DATE_EXECUTION, current: date(2000, 4, 20), next: date(2001, 4, 15, 7, 3) },
  { info: MONTHLY_WEEKDAY_EXECUTION, current: date(2000, 4, 1), next: date(2000, 4, 10, 7, 3) },
  { info: MONTHLY_WEEKDAY_EXECUTION, current: date(2000, 4, 20), next: date(2000, 5, 8, 7, 3) },
  { info: MONTHLY_WEEKDAY_EXECUTION, current: date(2000, 5, 20), next: date(2001, 4, 9, 7, 3) },
  { info: APRIL_WEEKDAY_EXECUTION, current: date(2000, 4, 20), next: date(2001, 4, 9, 7, 3) },
] satisfies readonly {
  info: ProjectExecutionInfo
  current: ZonedDateTime
  next: ZonedDateTime
}[])(
  'Get first project execution date on or after ($info.repeat.type, current: $current)',
  ({ info, current, next }) => {
    v.expect(firstProjectExecutionOnOrAfter(info, current).toString()).toBe(next.toString())
  },
)

v.test.each([
  {
    info: APRIL_DATE_EXECUTION,
    start: date(2000, 4, 1),
    end: date(2000, 5, 1),
    expected: [date(2000, 4, 15, 7, 3)],
  },
  {
    info: APRIL_DATE_EXECUTION,
    start: date(2000, 1, 1),
    end: date(2002, 1, 1),
    expected: [date(2000, 4, 15, 7, 3), date(2001, 4, 15, 7, 3)],
  },
  {
    info: MONTHLY_DATE_EXECUTION,
    start: date(2000, 4, 1),
    end: date(2000, 6, 1),
    expected: [],
  },
  {
    info: MONTHLY_WEEKDAY_EXECUTION,
    start: date(2000, 4, 1),
    end: date(2000, 7, 1),
    expected: [date(2000, 4, 10, 7, 3), date(2000, 5, 8, 7, 3)],
  },
] satisfies readonly {
  info: ProjectExecutionInfo
  start: ZonedDateTime
  end: ZonedDateTime
  expected: readonly ZonedDateTime[]
}[])(
  'Get project execution repetitions in range ($info.repeat.type, $start to $end)',
  ({ info, start, end, expected }) => {
    v.expect(
      getProjectExecutionRepetitionsForDateRange(info, start, end).map((d) => d.toString()),
    ).toEqual(expected.map((d) => d.toString()))
  },
)
