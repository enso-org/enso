import { ZonedDateTime, parseAbsolute } from '@internationalized/date'
import { DAYS_PER_WEEK, MONTHS_PER_YEAR, getDay } from '../../utilities/data/dateTime.js'
import type { ProjectExecutionInfo } from '../Backend.js'

/** Positive modulo of the number with respect to the base. */
function remainder(n: number, mod: number) {
  return ((n % mod) + mod) % mod
}

/** The first execution date of the given {@link ProjectExecution} on or after the given date. */
export function firstProjectExecutionOnOrAfter(
  projectExecution: ProjectExecutionInfo,
  startDate: ZonedDateTime,
): ZonedDateTime {
  let nextDate = startDate
  const { repeat } = projectExecution
  const executionStartDate = parseAbsolute(
    new Date(projectExecution.startDate).toISOString(),
    startDate.timeZone,
  )
  if (nextDate.compare(executionStartDate) < 0) {
    nextDate = executionStartDate
  }
  nextDate = nextDate.set({
    hour: executionStartDate.hour,
    minute: executionStartDate.minute,
    second: executionStartDate.second,
  })
  if (nextDate.compare(startDate) < 0) {
    nextDate = nextDate.add({ days: 1 })
  }
  switch (repeat.type) {
    case 'monthlyDate':
    case 'monthlyWeekday': {
      const currentMonth = nextDate.month - 1
      const month = repeat.months.find((month) => month >= currentMonth) ?? repeat.months[0] ?? 0
      const monthOffset = remainder(month - currentMonth, MONTHS_PER_YEAR)
      nextDate = nextDate.add({ months: monthOffset })
    }
  }
  switch (repeat.type) {
    case 'none': {
      return parseAbsolute(projectExecution.startDate, startDate.timeZone)
    }
    case 'daily': {
      break
    }
    case 'weekly': {
      const currentDay = getDay(nextDate)
      const day = repeat.daysOfWeek.find((day) => day >= currentDay) ?? repeat.daysOfWeek[0] ?? 0
      const dayOffset = remainder(day - currentDay, DAYS_PER_WEEK)
      nextDate = nextDate.add({ days: dayOffset })
      break
    }
    case 'monthlyWeekday': {
      const currentDate = nextDate.day
      nextDate = nextDate.set({ day: 1 + (repeat.weekNumber - 1) * DAYS_PER_WEEK })
      const currentDay = getDay(nextDate)
      const dayOffset = (repeat.dayOfWeek - currentDay + 7) % 7
      nextDate = nextDate.add({ days: dayOffset })
      if (nextDate.day < currentDate) {
        nextDate = nextDate.set({ day: 1 })
        nextDate = nextDate.add({ months: 1 })
        nextDate = nextDate.set({ day: 1 + (repeat.weekNumber - 1) * DAYS_PER_WEEK })
        const currentDay = getDay(nextDate)
        const dayOffset = (repeat.dayOfWeek - currentDay + 7) % 7
        nextDate = nextDate.add({ days: dayOffset })
      }
      break
    }
    case 'monthlyDate': {
      const currentDate = nextDate.day
      const date = repeat.date
      const goToNextMonth = repeat.date < currentDate
      if (goToNextMonth) {
        const startMonth = nextDate.month
        const month = repeat.months.find((month) => month > startMonth) ?? repeat.months[0] ?? 0
        const monthOffset = remainder(month - startMonth, MONTHS_PER_YEAR)
        nextDate = nextDate.add({ months: monthOffset })
      }
      nextDate = nextDate.set({ day: date })
      break
    }
  }
  return nextDate
}

/** The next scheduled execution date of given {@link ProjectExecution}. */
export function nextProjectExecutionDate(
  projectExecution: ProjectExecutionInfo,
  date: ZonedDateTime,
): ZonedDateTime | null {
  let nextDate = date
  const { repeat } = projectExecution
  switch (repeat.type) {
    case 'monthlyDate':
    case 'monthlyWeekday': {
      const currentMonth = nextDate.month - 1
      const month = repeat.months.find((month) => month > currentMonth) ?? repeat.months[0] ?? 0
      const monthOffset = remainder(month - currentMonth, MONTHS_PER_YEAR)
      nextDate = nextDate.add({ months: monthOffset })
    }
  }
  switch (repeat.type) {
    case 'none':
    default: {
      return null
    }
    case 'daily': {
      nextDate = nextDate.add({ days: 1 })
      break
    }
    case 'weekly': {
      const currentDay = getDay(nextDate)
      const day = repeat.daysOfWeek.find((day) => day > currentDay) ?? repeat.daysOfWeek[0] ?? 0
      const dayOffset = ((day - currentDay + 6) % 7) + 1
      nextDate = nextDate.add({ days: dayOffset })
      break
    }
    case 'monthlyWeekday': {
      nextDate = nextDate.set({ day: 1 + (repeat.weekNumber - 1) * DAYS_PER_WEEK })
      const currentDay = getDay(nextDate)
      const dayOffset = remainder(repeat.dayOfWeek - currentDay - 1, DAYS_PER_WEEK) + 1
      nextDate = nextDate.add({ days: dayOffset })
      break
    }
    case 'monthlyDate': {
      nextDate = nextDate.set({ day: repeat.date })
      break
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
  if (projectExecution.repeat.type === 'none') {
    const soleExecutionDate = parseAbsolute(projectExecution.startDate, startDate.timeZone)
    const isSoleExecutionWithinRange =
      startDate.compare(soleExecutionDate) < 0 && endDate.compare(soleExecutionDate) > 0
    return isSoleExecutionWithinRange ? [soleExecutionDate] : []
  }
  const firstDate = firstProjectExecutionOnOrAfter(projectExecution, startDate)
  if (firstDate.compare(endDate) > 0) {
    return []
  }
  const repetitions: ZonedDateTime[] = [firstDate]
  let currentDate = nextProjectExecutionDate(projectExecution, firstDate)
  while (currentDate != null && currentDate.compare(endDate) < 0) {
    repetitions.push(currentDate)
    currentDate = nextProjectExecutionDate(projectExecution, currentDate)
  }
  return repetitions
}
