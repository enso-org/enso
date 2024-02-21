/** @file Utilities for manipulating and displaying dates and times. */
import * as newtype from '#/utilities/newtype'

// =================
// === Constants ===
// =================

/** The number of hours in half a day. This is used to get the number of hours for AM/PM time. */
const HALF_DAY_HOURS = 12

// ================
// === DateTime ===
// ================

/** A string with date and time, following the RFC3339 specification. */
export type Rfc3339DateTime = newtype.Newtype<string, 'Rfc3339DateTime'>
/** Create a {@link Rfc3339DateTime}. */
// This is a constructor function that constructs values of the type it is named after.
// eslint-disable-next-line @typescript-eslint/no-redeclare
export const Rfc3339DateTime = newtype.newtypeConstructor<Rfc3339DateTime>()

/** Format a {@link Date} into the preferred format: `YYYY-MM-DD, hh:mm`. */
export function formatDateTime(date: Date) {
  const year = date.getFullYear()
  const month = (date.getMonth() + 1).toString().padStart(2, '0')
  const dayOfMonth = date.getDate().toString().padStart(2, '0')
  const hour = date.getHours().toString().padStart(2, '0')
  const minute = date.getMinutes().toString().padStart(2, '0')
  return `${year}-${month}-${dayOfMonth}, ${hour}:${minute}`
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
