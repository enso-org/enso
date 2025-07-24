/** @file Utilities related to time. */
import type { Rfc3339DateTime } from 'enso-common/src/utilities/data/dateTime'
import { useEffect, useState } from 'react'

/** The number of milliseconds in a minute. */
export const MINUTE_MS = 60_000
/** The number of milliseconds in an hour. */
export const HOUR_MS = 3_600_000
/** The number of hours in a day. */
export const HOURS_PER_DAY = 24
/** The number of milliseconds in a day. */
export const DAY_MS = HOURS_PER_DAY * HOUR_MS

/** Return days left, hours left, and progress for a duration. */
export function rfc3339DurationProgress(
  start: Rfc3339DateTime,
  end: Rfc3339DateTime,
  now = new Date(),
) {
  const endDate = new Date(end)
  const startDate = new Date(start)
  const msToEnd = Number(endDate) - Number(now)
  const daysLeft = Math.max(0, Math.floor(msToEnd / DAY_MS))
  const hoursLeft = Math.max(0, Math.floor(msToEnd / HOUR_MS))
  const fraction = 1 - msToEnd / (Number(endDate) - Number(startDate))
  return {
    daysLeft,
    /** Total hours. This will be 24 or greater when `daysLeft` is above 0. */
    hoursLeft,
    fraction,
  }
}

/** A React hook that presents current timestamp as state value and updates in specified interval. */
export function useCurrentTimestamp(refreshInterval: number) {
  const [timestampValue, setTimestampValue] = useState(Date.now())
  useEffect(() => {
    const interval = setInterval(() => setTimestampValue(Date.now()), refreshInterval)
    return () => clearInterval(interval)
  }, [refreshInterval])
  return timestampValue
}
