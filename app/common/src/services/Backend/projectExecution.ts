import { EMPTY_ARRAY } from '../../utilities/data/array'
import { nativeDateToTimeZoneDate, timeZoneDateToNativeDate } from '../../utilities/data/dateTime'
import { ProjectExecutionInfo } from '../Backend'

const DAYS_PER_WEEK = 7
const MONTHS_PER_YEAR = 12

/** The first execution date of the given {@link ProjectExecution} on or after the given date. */
export function firstProjectExecutionOnOrAfter(
  projectExecution: ProjectExecutionInfo,
  startDate: Date,
  timeZone?: string,
): Date {
  let nextDate = nativeDateToTimeZoneDate(new Date(startDate), timeZone)
  const { repeat } = projectExecution
  const executionStartDate = nativeDateToTimeZoneDate(new Date(projectExecution.startDate))
  if (nextDate < executionStartDate) {
    nextDate = new Date(executionStartDate)
  }
  nextDate.setUTCHours(executionStartDate.getUTCHours())
  nextDate.setUTCMinutes(executionStartDate.getUTCMinutes())
  nextDate.setUTCSeconds(executionStartDate.getUTCSeconds())
  if (nextDate < startDate) {
    nextDate.setUTCDate(nextDate.getUTCDate() + 1)
  }
  switch (repeat.type) {
    case 'daily': {
      const currentDay = nextDate.getUTCDay()
      const day = repeat.daysOfWeek.find((day) => day >= currentDay) ?? repeat.daysOfWeek[0] ?? 0
      const dayOffset = (day - currentDay + DAYS_PER_WEEK) % DAYS_PER_WEEK
      nextDate.setUTCDate(nextDate.getUTCDate() + dayOffset)
      break
    }
    case 'monthly-weekday': {
      const currentDate = nextDate.getUTCDate()
      nextDate.setUTCDate(1)
      nextDate.setUTCDate(1 + (repeat.weekNumber - 1) * DAYS_PER_WEEK)
      const currentDay = nextDate.getUTCDay()
      const dayOffset = (repeat.dayOfWeek - currentDay + 7) % 7
      nextDate.setUTCDate(nextDate.getUTCDate() + dayOffset)
      if (nextDate.getUTCDate() < currentDate) {
        nextDate.setUTCDate(1)
        nextDate.setUTCMonth(nextDate.getUTCMonth() + 1)
        nextDate.setUTCDate(1 + (repeat.weekNumber - 1) * DAYS_PER_WEEK)
        const currentDay = nextDate.getUTCDay()
        const dayOffset = (repeat.dayOfWeek - currentDay + 7) % 7
        nextDate.setUTCDate(nextDate.getUTCDate() + dayOffset)
      }
      break
    }
    case 'monthly-date': {
      const currentDate = nextDate.getUTCDate()
      const date = repeat.date
      const goToNextMonth = date < currentDate
      nextDate.setUTCDate(date)
      if (goToNextMonth) {
        const startMonth = nextDate.getUTCMonth()
        nextDate.setUTCMonth(startMonth + 1)
        if ((nextDate.getUTCMonth() + MONTHS_PER_YEAR - startMonth) % MONTHS_PER_YEAR > 1) {
          nextDate.setUTCDate(0)
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
      const currentMonth = nextDate.getUTCMonth()
      const month = repeat.months.find((month) => month >= currentMonth) ?? repeat.months[0] ?? 0
      const monthOffset = (month - currentMonth + MONTHS_PER_YEAR) % MONTHS_PER_YEAR
      nextDate.setUTCMonth(nextDate.getUTCMonth() + monthOffset)
    }
  }
  return timeZoneDateToNativeDate(nextDate, timeZone)
}

/** The next scheduled execution date of given {@link ProjectExecution}. */
export function nextProjectExecutionDate(
  projectExecution: ProjectExecutionInfo,
  date: Date,
  timeZone?: string,
): Date {
  const nextDate = nativeDateToTimeZoneDate(new Date(date), timeZone)
  const { repeat } = projectExecution
  switch (repeat.type) {
    case 'daily': {
      const currentDay = nextDate.getUTCDay()
      const day = repeat.daysOfWeek.find((day) => day > currentDay) ?? repeat.daysOfWeek[0] ?? 0
      const dayOffset = ((day - currentDay + 6) % 7) + 1
      nextDate.setUTCDate(nextDate.getUTCDate() + dayOffset)
      break
    }
    case 'monthly-weekday': {
      nextDate.setUTCDate(1)
      nextDate.setUTCMonth(nextDate.getUTCMonth() + 1)
      nextDate.setUTCDate(1 + (repeat.weekNumber - 1) * DAYS_PER_WEEK)
      const currentDay = nextDate.getUTCDay()
      const dayOffset = ((repeat.dayOfWeek - currentDay + 6) % 7) + 1
      nextDate.setUTCDate(nextDate.getUTCDate() + dayOffset)
      break
    }
    case 'monthly-date': {
      const startMonth = nextDate.getUTCMonth()
      nextDate.setUTCMonth(startMonth + 1)
      if ((nextDate.getUTCMonth() + MONTHS_PER_YEAR - startMonth) % MONTHS_PER_YEAR > 1) {
        nextDate.setUTCDate(0)
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
      const currentMonth = nextDate.getUTCMonth()
      const month = repeat.months.find((month) => month >= currentMonth) ?? repeat.months[0] ?? 0
      const monthOffset = (month - currentMonth + MONTHS_PER_YEAR) % MONTHS_PER_YEAR
      nextDate.setUTCMonth(nextDate.getUTCMonth() + monthOffset)
    }
  }
  return timeZoneDateToNativeDate(nextDate, timeZone)
}

/**
 * All executions of the given {@link ProjectExecution} between the given dates.
 * By default, return an empty array if the {@link ProjectExecution} repeats hourly.
 * This is to prevent UI from being overly cluttered.
 */
export function getProjectExecutionRepetitionsForDateRange(
  projectExecution: ProjectExecutionInfo,
  startDate: Date,
  endDate: Date,
  timeZone?: string,
): readonly Date[] {
  const firstDate = firstProjectExecutionOnOrAfter(projectExecution, startDate, timeZone)
  if (firstDate >= endDate) {
    return EMPTY_ARRAY
  }
  const repetitions: Date[] = [firstDate]
  let currentDate = firstDate
  currentDate = nextProjectExecutionDate(projectExecution, currentDate, timeZone)
  while (currentDate < endDate) {
    repetitions.push(currentDate)
    currentDate = nextProjectExecutionDate(projectExecution, currentDate, timeZone)
  }
  return repetitions
}
