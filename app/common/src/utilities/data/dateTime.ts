/** @file Utilities for manipulating and displaying dates and times. */
import type { TextId } from '../../text'
import { type Newtype, newtypeConstructor } from './newtype'

/** The number of hours in half a day. This is used to get the number of hours for AM/PM time. */
export const HALF_DAY_HOURS = 12
/** The number of milliseconds in one minute. */
export const MINUTE_MS = 60_000
export const MAX_DAYS_PER_MONTH = 31
export const DAYS_PER_WEEK = 7
export const HOURS_PER_DAY = 24
export const HOUR_MINUTE = 60

/** A mapping from the month index returned by {@link Date.getMonth} to its full name. */
export const MONTH_NAMES = [
  'January',
  'February',
  'March',
  'April',
  'May',
  'June',
  'July',
  'August',
  'September',
  'October',
  'November',
  'December',
]

export const DAY_3_LETTER_TEXT_IDS = [
  'sunday3',
  'monday3',
  'tuesday3',
  'wednesday3',
  'thursday3',
  'friday3',
  'saturday3',
] satisfies TextId[]

export const DAY_TEXT_IDS = [
  'sunday',
  'monday',
  'tuesday',
  'wednesday',
  'thursday',
  'friday',
  'saturday',
] satisfies TextId[]

export const MONTH_3_LETTER_TEXT_IDS = [
  'january3',
  'february3',
  'march3',
  'april3',
  'may3',
  'june3',
  'july3',
  'august3',
  'september3',
  'october3',
  'november3',
  'december3',
] satisfies TextId[]

/** A string with date and time, following the RFC3339 specification. */
export type Rfc3339DateTime = Newtype<string, 'Rfc3339DateTime'>
/** Create a {@link Rfc3339DateTime}. */
export const Rfc3339DateTime = newtypeConstructor<Rfc3339DateTime>()

/**
 * Return a new {@link Date} with units below days (hours, minutes, seconds and milliseconds)
 * set to `0`.
 */
export function toDate(dateTime: Date) {
  return new Date(dateTime.getFullYear(), dateTime.getMonth(), dateTime.getDate())
}

/** Format a {@link Date} into the preferred date format: `YYYY-MM-DD`. */
export function formatDate(date: Date) {
  const year = date.getFullYear()
  const month = (date.getMonth() + 1).toString().padStart(2, '0')
  const dayOfMonth = date.getDate().toString().padStart(2, '0')
  return `${year}-${month}-${dayOfMonth}`
}

/** Format a {@link Date} into the preferred date-time format: `YYYY-MM-DD, hh:mm`. */
export function formatDateTime(date: Date) {
  const hour = date.getHours().toString().padStart(2, '0')
  const minute = date.getMinutes().toString().padStart(2, '0')
  return `${formatDate(date)}, ${hour}:${minute}`
}

/** Format a {@link Date} into the preferred chat-frienly format: `DD/MM/YYYY, hh:mm PM`. */
export function formatDateTimeChatFriendly(date: Date) {
  const year = date.getFullYear()
  const month = (date.getMonth() + 1).toString().padStart(2, '0')
  const dayOfMonth = date.getDate().toString().padStart(2, '0')
  let hourRaw = date.getHours()
  let amOrPm = 'AM'
  if (hourRaw > HALF_DAY_HOURS) {
    hourRaw -= HALF_DAY_HOURS
    amOrPm = 'PM'
  }
  const hour = hourRaw.toString().padStart(2, '0')
  const minute = date.getMinutes().toString().padStart(2, '0')
  return `${dayOfMonth}/${month}/${year} ${hour}:${minute} ${amOrPm}`
}

/** Format a {@link Date} as a {@link Rfc3339DateTime}. */
export function toRfc3339(date: Date) {
  return Rfc3339DateTime(date.toISOString())
}

/** Convert a UTC date to a local date. */
export function localDateToUtcDate(date: Date) {
  return new Date(Number(date) + date.getTimezoneOffset() * MINUTE_MS)
}

/** Convert a local date to a UTC date. */
export function utcDateToLocalDate(date: Date) {
  return new Date(Number(date) - date.getTimezoneOffset() * MINUTE_MS)
}
