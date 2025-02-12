import * as v from 'vitest'
import { toRfc3339 } from '../../../utilities/data/dateTime'
import { ProjectExecutionInfo, ProjectId } from '../../Backend'
import { firstProjectExecutionOnOrAfter, nextProjectExecutionDate } from '../projectExecution'

const DAILY_EXECUTION: ProjectExecutionInfo = {
  projectId: ProjectId('project-aaaaaaaa'),
  repeat: {
    type: 'daily',
    daysOfWeek: [0, 5],
  },
  startDate: toRfc3339(new Date(2000, 0, 1, 7, 3)),
  timeZone: 'UTC',
  maxDurationMinutes: 60,
  parallelMode: 'ignore',
}

v.test.each([
  {
    info: DAILY_EXECUTION,
    current: new Date(2000, 5, 4, 7, 3),
    next1: new Date(2000, 5, 9, 7, 3),
    next2: new Date(2000, 5, 11, 7, 3),
    next3: new Date(2000, 5, 16, 7, 3),
  },
] satisfies readonly {
  info: ProjectExecutionInfo
  current: Date
  next1: Date
  next2: Date
  next3: Date
}[])(
  'Get next project execution date (current: $current)',
  ({ info, current, next1, next2, next3 }) => {
    v.expect(nextProjectExecutionDate(info, current)).toStrictEqual(next1)
    v.expect(nextProjectExecutionDate(info, next1)).toStrictEqual(next2)
    v.expect(nextProjectExecutionDate(info, next2)).toStrictEqual(next3)
  },
)

v.test.each([
  { info: DAILY_EXECUTION, current: new Date(1999, 1, 1), next: new Date(2000, 0, 2, 7, 3) },
  { info: DAILY_EXECUTION, current: new Date(2000, 10, 16), next: new Date(2000, 10, 17, 7, 3) },
] satisfies readonly {
  info: ProjectExecutionInfo
  current: Date
  next: Date
}[])(
  'Get first project execution date on or after (current: $current)',
  ({ info, current, next }) => {
    v.expect(firstProjectExecutionOnOrAfter(info, current)).toStrictEqual(next)
  },
)
