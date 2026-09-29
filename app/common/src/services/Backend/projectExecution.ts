import { ZonedDateTime, parseAbsolute } from '@internationalized/date'
import { DAYS_PER_WEEK, MONTHS_PER_YEAR, getDay } from '../../utilities/data/dateTime.js'
import type {
  ProjectExecutionInfo,
  ProjectExecutionMonthlyDateRepeatInfo,
  ProjectExecutionMonthlyWeekdayRepeatInfo,
} from '../Backend.js'

/** Positive modulo of the number with respect to the base. */
function remainder(n: number, mod: number) {
  return ((n % mod) + mod) % mod
}

/** Repeat info for the repeat types that are restricted to a set of months. */
type MonthlyRepeatInfo =
  | ProjectExecutionMonthlyDateRepeatInfo
  | ProjectExecutionMonthlyWeekdayRepeatInfo

/** The execution date of a monthly repeat within the month of the given date. */
function monthlyExecutionInMonth(repeat: MonthlyRepeatInfo, dateInMonth: ZonedDateTime) {
  switch (repeat.type) {
    case 'monthlyDate': {
      return dateInMonth.set({ day: repeat.date })
    }
    case 'monthlyWeekday': {
      const weekStart = dateInMonth.set({ day: 1 + (repeat.weekNumber - 1) * DAYS_PER_WEEK })
      const dayOffset = remainder(repeat.dayOfWeek - getDay(weekStart), DAYS_PER_WEEK)
      return weekStart.add({ days: dayOffset })
    }
  }
}

/**
 * The first execution date of a monthly repeat on or after the given date, looking no earlier
 * than `minMonthOffset` months after the month of the given date.
 * Months in `repeat.months` are zero-based (January is `0`).
 */
function firstMonthlyExecutionOnOrAfter(
  repeat: MonthlyRepeatInfo,
  date: ZonedDateTime,
  minMonthOffset: number,
) {
  const months = repeat.months.length > 0 ? repeat.months : [0]
  const monthStart = date.set({ day: 1 })
  // One full year past the first candidate month always contains an execution.
  for (let offset = minMonthOffset; offset <= minMonthOffset + MONTHS_PER_YEAR; offset += 1) {
    const dateInMonth = monthStart.add({ months: offset })
    if (!months.includes(dateInMonth.month - 1)) continue
    const execution = monthlyExecutionInMonth(repeat, dateInMonth)
    if (execution.compare(date) >= 0) return execution
  }
  return null
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
    case 'monthlyWeekday':
    case 'monthlyDate': {
      nextDate = firstMonthlyExecutionOnOrAfter(repeat, nextDate, 0) ?? nextDate
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
    case 'monthlyWeekday':
    case 'monthlyDate': {
      // Each month has at most one execution, so the next one is in a later month.
      return firstMonthlyExecutionOnOrAfter(repeat, nextDate, 1)
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
