/** @file Utilities for manipulating and displaying dates and times */
import * as newtype from '../newtype'

// ================
// === DateTime ===
// ================

/** A string with date and time, following the RFC3339 specification. */
export type Rfc3339DateTime = newtype.Newtype<string, 'Rfc3339DateTime'>

/** Formats date time into the preferred format: `YYYY-MM-DD, hh:mm`. */
export function formatDateTime(date: Date) {
    const year = date.getFullYear()
    const month = (date.getMonth() + 1).toString().padStart(2, '0')
    const dayOfMonth = date.getDate().toString().padStart(2, '0')
    const hour = date.getHours().toString().padStart(2, '0')
    const minute = date.getMinutes().toString().padStart(2, '0')
    return `${year}-${month}-${dayOfMonth}, ${hour}:${minute}`
}

/** Formats a {@link Date} as a {@link Rfc3339DateTime}  */
export function toRfc3339(date: Date) {
    return newtype.asNewtype<Rfc3339DateTime>(date.toISOString())
}
