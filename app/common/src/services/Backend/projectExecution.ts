import { ZonedDateTime, getDayOfWeek, parseZonedDateTime, toZoned } from '@internationalized/date'
import { EMPTY_ARRAY } from '../../utilities/data/array'
import { ProjectExecutionInfo } from '../Backend'

const DAYS_PER_WEEK = 7
const MONTHS_PER_YEAR = 12

/** The first execution date of the given {@link ProjectExecution} on or after the given date. */
export function firstProjectExecutionOnOrAfter(
  projectExecution: ProjectExecutionInfo,
  startDate: ZonedDateTime,
): ZonedDateTime {
  let nextDate = startDate
  const { repeat } = projectExecution
  const executionStartDate = toZoned(
    parseZonedDateTime(projectExecution.startDate),
    startDate.timeZone,
  )
  if (nextDate < executionStartDate) {
    nextDate = executionStartDate
  }
  nextDate.set({
    hour: executionStartDate.hour,
    minute: executionStartDate.minute,
    second: executionStartDate.second,
  })
  if (nextDate.compare(startDate) < 0) {
    nextDate.add({ days: 1 })
  }
  switch (repeat.type) {
    case 'daily': {
      const currentDay = getDayOfWeek(nextDate, 'en-US')
      const day = repeat.daysOfWeek.find((day) => day >= currentDay) ?? repeat.daysOfWeek[0] ?? 0
      const dayOffset = (day - currentDay + DAYS_PER_WEEK) % DAYS_PER_WEEK
      nextDate.add({ days: dayOffset })
      break
    }
    case 'monthly-weekday': {
      const currentDate = nextDate.day
      nextDate.set({ day: 1 + (repeat.weekNumber - 1) * DAYS_PER_WEEK })
      const currentDay = getDayOfWeek(nextDate, 'en-US')
      const dayOffset = (repeat.dayOfWeek - currentDay + 7) % 7
      nextDate.add({ days: dayOffset })
      if (nextDate.day < currentDate) {
        nextDate.set({ day: 1 })
        nextDate.add({ months: 1 })
        nextDate.set({ day: 1 + (repeat.weekNumber - 1) * DAYS_PER_WEEK })
        const currentDay = getDayOfWeek(nextDate, 'en-US')
        const dayOffset = (repeat.dayOfWeek - currentDay + 7) % 7
        nextDate.add({ days: dayOffset })
      }
      break
    }
    case 'monthly-date': {
      const currentDate = nextDate.day
      const date = repeat.date
      const goToNextMonth = date < currentDate
      nextDate.set({ day: date })
      if (goToNextMonth) {
        const startMonth = nextDate.month
        nextDate.add({ months: 1 })
        if ((nextDate.month + MONTHS_PER_YEAR - startMonth) % MONTHS_PER_YEAR > 1) {
          nextDate.set({ day: 0 })
        }
      }
      break
    }
  }
  switch (repeat.type) {
    case 'daily': {
      break
    }
    case 'monthly-date':
    case 'monthly-weekday': {
      const currentMonth = nextDate.month
      const month = repeat.months.find((month) => month >= currentMonth) ?? repeat.months[0] ?? 0
      const monthOffset = (month - currentMonth + MONTHS_PER_YEAR) % MONTHS_PER_YEAR
      nextDate.add({ months: monthOffset })
    }
  }
  return nextDate
}

/** The next scheduled execution date of given {@link ProjectExecution}. */
export function nextProjectExecutionDate(
  projectExecution: ProjectExecutionInfo,
  date: ZonedDateTime,
): ZonedDateTime {
  const nextDate = date
  const { repeat } = projectExecution
  switch (repeat.type) {
    case 'daily': {
      const currentDay = getDayOfWeek(nextDate, 'en-US')
      const day = repeat.daysOfWeek.find((day) => day > currentDay) ?? repeat.daysOfWeek[0] ?? 0
      const dayOffset = ((day - currentDay + 6) % 7) + 1
      nextDate.set({ day: dayOffset })
      break
    }
    case 'monthly-weekday': {
      nextDate.set({ day: 1 })
      nextDate.add({ months: 1 })
      nextDate.set({ day: 1 + (repeat.weekNumber - 1) * DAYS_PER_WEEK })
      const currentDay = getDayOfWeek(nextDate, 'en-US')
      const dayOffset = ((repeat.dayOfWeek - currentDay + 6) % 7) + 1
      nextDate.add({ days: dayOffset })
      break
    }
    case 'monthly-date': {
      const startMonth = nextDate.month
      nextDate.add({ months: 1 })
      if ((nextDate.month + MONTHS_PER_YEAR - startMonth) % MONTHS_PER_YEAR > 1) {
        nextDate.set({ day: 0 })
      }
      break
    }
  }
  switch (repeat.type) {
    case 'daily': {
      break
    }
    case 'monthly-date':
    case 'monthly-weekday': {
      const currentMonth = nextDate.month
      const month = repeat.months.find((month) => month >= currentMonth) ?? repeat.months[0] ?? 0
      const monthOffset = (month - currentMonth + MONTHS_PER_YEAR) % MONTHS_PER_YEAR
      nextDate.add({ months: monthOffset })
    }
  }
  return nextDate
}

/**
 * All executions of the given {@link ProjectExecution} between the given dates.
 * By default, return an empty array if the {@link ProjectExecution} repeats hourly.
 * This is to prevent UI from being overly cluttered.
 */
export function getProjectExecutionRepetitionsForDateRange(
  projectExecution: ProjectExecutionInfo,
  startDate: ZonedDateTime,
  endDate: ZonedDateTime,
): readonly ZonedDateTime[] {
  const firstDate = firstProjectExecutionOnOrAfter(projectExecution, startDate)
  if (firstDate >= endDate) {
    return EMPTY_ARRAY
  }
  const repetitions: ZonedDateTime[] = [firstDate]
  let currentDate = firstDate
  currentDate = nextProjectExecutionDate(projectExecution, currentDate)
  while (currentDate < endDate) {
    repetitions.push(currentDate)
    currentDate = nextProjectExecutionDate(projectExecution, currentDate)
  }
  return repetitions
}
