/** @file Utilities for manipulating and displaying dates and times */
import * as backend from './backend'
import * as newtype from '../newtype'

export function formatDateTime(date: Date) {
    const year = date.getFullYear()
    const month = date.getMonth().toString().padStart(2, '0')
    const dayOfMonth = date.getDate().toString().padStart(2, '0')
    const hour = date.getHours().toString().padStart(2, '0')
    const minute = date.getMinutes().toString().padStart(2, '0')
    return `${year}-${month}-${dayOfMonth}, ${hour}:${minute}`
}

export function toRfc3339(date: Date) {
    return newtype.asNewtype<backend.Rfc3339DateTime>(date.toISOString())
}
