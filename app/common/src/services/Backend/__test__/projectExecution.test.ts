import * as v from 'vitest'

v.test('Test suite disabled (FIXME: #12426)', () => {})
/*
import { ZonedDateTime } from '@internationalized/date'
import * as v from 'vitest'
import { IanaTimeZone, toRfc3339 } from '../../../utilities/data/dateTime'
import { ProjectExecutionInfo, ProjectId } from '../../Backend'
import { firstProjectExecutionOnOrAfter, nextProjectExecutionDate } from '../projectExecution'

const TIME_ZONE = 'America/Los_Angeles'
const TIME_ZONE_WINTER_OFFSET = -28800000
const TIME_ZONE_SUMMER_OFFSET = -25200000

const WEEKLY_EXECUTION: ProjectExecutionInfo = {
  projectId: ProjectId('project-aaaaaaaa'),
  repeat: {
    type: 'weekly',
    daysOfWeek: [0, 5],
  },
  startDate: toRfc3339(
    new ZonedDateTime(2000, 1, 1, TIME_ZONE, TIME_ZONE_WINTER_OFFSET, 7, 3).toDate(),
  ),
  endDate: null,
  timeZone: IanaTimeZone('UTC'),
  maxDurationMinutes: 60,
  parallelMode: 'ignore',
}

v.test.each([
  {
    info: WEEKLY_EXECUTION,
    current: new ZonedDateTime(2000, 6, 4, TIME_ZONE, TIME_ZONE_SUMMER_OFFSET, 7, 3),
    next1: new ZonedDateTime(2000, 6, 9, TIME_ZONE, TIME_ZONE_SUMMER_OFFSET, 7, 3),
    next2: new ZonedDateTime(2000, 6, 11, TIME_ZONE, TIME_ZONE_SUMMER_OFFSET, 7, 3),
    next3: new ZonedDateTime(2000, 6, 16, TIME_ZONE, TIME_ZONE_SUMMER_OFFSET, 7, 3),
  },
] satisfies readonly {
  info: ProjectExecutionInfo
  current: ZonedDateTime
  next1: ZonedDateTime
  next2: ZonedDateTime
  next3: ZonedDateTime
}[])(
  'Get next project execution date (current: $current)',
  ({ info, current, next1, next2, next3 }) => {
    v.expect(nextProjectExecutionDate(info, current)?.toString()).toBe(next1.toString())
    v.expect(nextProjectExecutionDate(info, next1)?.toString()).toBe(next2.toString())
    v.expect(nextProjectExecutionDate(info, next2)?.toString()).toBe(next3.toString())
  },
)

v.test.each([
  {
    info: WEEKLY_EXECUTION,
    current: new ZonedDateTime(1999, 1, 1, TIME_ZONE, TIME_ZONE_WINTER_OFFSET),
    next: new ZonedDateTime(2000, 1, 2, TIME_ZONE, TIME_ZONE_WINTER_OFFSET, 7, 3),
  },
  {
    info: WEEKLY_EXECUTION,
    current: new ZonedDateTime(2000, 11, 16, TIME_ZONE, TIME_ZONE_WINTER_OFFSET),
    next: new ZonedDateTime(2000, 11, 17, TIME_ZONE, TIME_ZONE_WINTER_OFFSET, 7, 3),
  },
] satisfies readonly {
  info: ProjectExecutionInfo
  current: ZonedDateTime
  next: ZonedDateTime
}[])(
  'Get first project execution date on or after (current: $current)',
  ({ info, current, next }) => {
    v.expect(firstProjectExecutionOnOrAfter(info, current).toString()).toBe(next.toString())
  },
)
 */
