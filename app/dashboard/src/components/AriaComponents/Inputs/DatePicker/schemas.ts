/** @file Schemas related to `DatePicker`. */
import * as z from 'zod'

export const ZONED_DATE_TIME_SCHEMA = z.object({
  calendar: z.object({}),
  era: z.string(),
  year: z.number().int(),
  month: z.number().int(),
  day: z.number().int(),
  hour: z.number().int(),
  minute: z.number().int(),
  second: z.number().int(),
  millisecond: z.number().int(),
  timezone: z.string(),
  offset: z.number().int(),
})
